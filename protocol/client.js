// Persistent `blade ide serve` client. This is a plain request/response data
// pipe: `Blade.exe ide serve` reads one JSON object per stdin line and writes
// one JSON object per stdout line, correlated by an integer "id"
// (./serveProto.js owns that framing so a protocol test can exercise it
// without owning a process). No pty, no echo, no :paste framing — cp.spawn
// straight to stdin/stdout.
//
// Two things a plain `blade repl` session does NOT need that this module does:
//   - A capability probe. Unlike `blade repl` (which has existed since before
//     the JSON IDE mode), `ide serve` may not exist on the compiler on
//     $PATH — the first check() lazily spawns the process and pings it; a
//     clean {ok:true} within PING_TIMEOUT_MS latches `available()` to "yes",
//     a spawn failure or ping timeout latches it to "no" for the rest of the
//     session (don't hammer a compiler that doesn't have the subcommand).
//   - Explicit restart-with-backoff. A REPL session dying mid-edit is
//     unrecoverable anyway (the accumulated bindings are gone). A `serve`
//     process dying is just a hiccup — it carries no state across requests
//     (Ide.fs resets its IDE side-channels per request) — so a request
//     timeout kills the process and the NEXT check() call respawns it,
//     subject to a backoff floor (500ms / 2s / 8s) so a wedged compiler
//     can't be respawned on every 300ms keystroke. After
//     MAX_ESTABLISHED_FAILURES consecutive failures once serve has proven
//     itself, we give up for the session exactly like the initial probe.
//
// Per-request timeouts are deliberately generous tier-dependent defaults
// (fast/full) since "full" runs monomorphization; callers may override per
// call.
//
// createClient() and its process: all of the above (spawn, ping probe, id
// correlation, timeouts, backoff, dispose) lives in ONE process's worth of
// private state per client instance. Two clients never share a process or any
// other mutable state — each is a fully self-contained closure. Callers that
// want a long-lived shared client (an editor's fast/slow check clocks) and
// callers that want their OWN process (a notebook, whose cell eval can invoke
// a multi-second g++ fallback that would otherwise stall every keystroke's
// fast-tier check) simply call createClient() the number of times they need.
// Host lifecycle wiring — singletons, subscription registration, activation —
// deliberately lives in the CONSUMER, not here.

"use strict";

const cp = require("child_process");
const proto = require("./serveProto");
const display = require("./display");

const PING_TIMEOUT_MS = 5000;
const DEFAULT_TIMEOUT_MS = { fast: 10000, full: 30000 };
const BACKOFF_MS = [500, 2000, 8000];
const MAX_ESTABLISHED_FAILURES = 3;

/** The subcommand this client speaks. Overridable per client via `deps.args`
 *  (a test seam: a fake NDJSON server is spawned as `node fake-serve.js`). */
const DEFAULT_ARGS = ["ide", "serve"];

/**
 * Build one `ide serve` client. `dependencies` is
 * `{ findCompiler, output?, cwd?, args?, env? }`:
 *
 *   - `findCompiler()` (REQUIRED) returns the compiler path to spawn. Called
 *     at EVERY spawn, not once — so a client re-resolves after a rebuild
 *     (see ./resolveCompiler.js, whose newest-mtime rule depends on this).
 *   - `output` (optional) is `{appendLine(line)}` — where this client's log
 *     lines go. Omitted means silence.
 *   - `cwd` (optional) is the working directory to spawn the child in, as a
 *     string or a function returning one (a function is re-read per spawn,
 *     which is what lets a host track a folder that can change). Undefined
 *     inherits this process's cwd. Note the compiler ALSO chdirs per request
 *     (to the checked file's directory, or an eval's `cwd`), so this is only
 *     the initial value.
 *   - `args` (optional) replaces `["ide","serve"]` as the spawn arguments.
 *   - `env` (optional) is the child's environment, as an object or a function
 *     returning one (a function is called at EVERY spawn, the same rule
 *     `findCompiler` follows, so a host can pick up a settings change without
 *     re-creating the client). Undefined — the default, and what every caller
 *     got before this hook existed — inherits this process's environment
 *     wholesale. NOTE that an object REPLACES the environment rather than
 *     extending it (that is Node's spawn semantics, not ours): a caller adding
 *     one variable must spread the parent's, e.g.
 *     `env: () => ({ ...process.env, GRDIR: grRoot })`. The compiler needs
 *     exactly that for GR renders — `renderPlot` spawns a native worker that
 *     resolves its DLLs off `GRDIR`/`PATH`.
 *
 * `label` (optional, default "blade serve") tags this client's lines in the
 * output channel, so a second client (e.g. a notebook's dedicated process)
 * doesn't read as the same process in the log.
 *
 * Returns `{ available, check, checkCells, eval, resetSession, renderPlot,
 * dispose }` — see the matching functions below for behavior. All mutable
 * state (proc, pending requests, availability latch, backoff bookkeeping) is
 * private to the returned client.
 */
function createClient(dependencies, label) {
  const deps = dependencies;
  const tag = label || "blade serve";

  /** @type {import("child_process").ChildProcess | undefined} */
  let proc;
  let nextId = 1;
  /** @type {Map<number, { resolve: (msg: object) => void, reject: (err: Error) => void }>} */
  let pending = new Map();

  // "unknown" until the first ping resolves or fails; then "yes" or "no".
  let availability = "unknown";
  // Has a ping EVER succeeded this client? Governs failure handling: the
  // very first probe latches straight to "no" on any failure (one-shot);
  // once serve has proven itself once, later failures get
  // MAX_ESTABLISHED_FAILURES backoff retries before giving up the same way.
  let established = false;
  let consecutiveFailures = 0;
  let nextSpawnAllowedAt = 0;
  // Shares one spawn+ping handshake across concurrent check()/eval() calls
  // that all land before the first probe resolves.
  let handshake = null;

  function log(line) {
    if (deps && deps.output) deps.output.appendLine(`[${tag}] ${line}`);
  }

  /** The child's working directory: `deps.cwd` as a string, or the result of
   *  calling it when it's a function (re-read per spawn). Undefined — the
   *  default — inherits this process's cwd. */
  function spawnCwd() {
    const c = deps && deps.cwd;
    if (typeof c === "function") return c() || undefined;
    return c || undefined;
  }

  /** The child's argv. `deps.args` wins when it is an array. */
  function spawnArgs() {
    return deps && Array.isArray(deps.args) ? deps.args : DEFAULT_ARGS;
  }

  /** The child's environment: `deps.env` as an object, or the result of
   *  calling it when it's a function (re-read per spawn, exactly like
   *  `findCompiler` and `cwd`). Undefined hands cp.spawn no `env` at all,
   *  which is inheritance — the pre-existing behavior, unchanged for every
   *  caller that doesn't set this. */
  function spawnEnv() {
    const e = deps && deps.env;
    if (typeof e === "function") return e() || undefined;
    return e || undefined;
  }

  // --- Failure / backoff bookkeeping ------------------------------------------

  function recordFailure(reason) {
    consecutiveFailures++;
    const giveUpAt = established ? MAX_ESTABLISHED_FAILURES : 1;
    if (consecutiveFailures >= giveUpAt) {
      availability = "no";
      log(`${reason} — giving up on 'ide serve' for this session (re-create the client to retry)`);
    } else {
      const backoff = BACKOFF_MS[Math.min(consecutiveFailures - 1, BACKOFF_MS.length - 1)];
      nextSpawnAllowedAt = Date.now() + backoff;
      log(`${reason} — retrying in ${backoff}ms (failure ${consecutiveFailures}/${giveUpAt})`);
    }
  }

  function recordSuccess() {
    established = true;
    availability = "yes";
    consecutiveFailures = 0;
  }

  // --- Process lifecycle -------------------------------------------------------

  function rejectAllPending(reason) {
    const err = new Error(`blade ide serve: ${reason}`);
    for (const [, p] of pending) p.reject(err);
    pending.clear();
  }

  /** Kill the current process (if any) and reject everything in flight.
   *  `isFailure` runs the backoff/give-up bookkeeping; pass false for a
   *  deliberate dispose(), which is not a failure. Idempotent — a second call
   *  while already torn down is a no-op, so racing failure paths (e.g. a
   *  request timeout that kills the process right as it happens to exit on
   *  its own) can't double-count. */
  function teardown(reason, isFailure) {
    if (!proc) return;
    const p = proc;
    proc = undefined;
    p.removeAllListeners();
    if (p.stdout) p.stdout.removeAllListeners();
    if (p.stderr) p.stderr.removeAllListeners();
    if (p.exitCode === null && p.signalCode === null) {
      try {
        p.kill();
      } catch (_) {
        /* already gone */
      }
    }
    rejectAllPending(reason);
    if (isFailure) recordFailure(reason);
  }

  function handleStdout(decoder, chunk) {
    const messages = decoder.push(chunk);
    for (const msg of messages) {
      // Out-of-band events are NOT responses. A line carrying "event" never
      // settles a pending request, even when it echoes that request's id —
      // otherwise a mid-eval display frame would resolve the eval early with
      // a payload that has no stdout/bindings (docs/display-frames.md).
      if (display.isEvent(msg)) {
        handleEvent(msg);
        continue;
      }
      const id = msg.id;
      if (id === undefined || id === null) {
        if (msg.error) log(`serve error (no id): ${msg.error}`);
        continue;
      }
      const p = pending.get(id);
      if (!p) continue; // already timed out, or an id we never sent — drop
      pending.delete(id);
      if (msg.error) {
        // Tag distinctly from a transport failure (timeout/spawn/crash): the
        // process answered LIVE with "I don't understand this request" —
        // e.g. an old compiler that predates eval/resetSession/surface.
        // Callers that care use this to stop retrying a command the compiler
        // will never support, without over-reacting to it (this is NOT
        // routed through recordFailure/teardown — the process is fine).
        const err = new Error(msg.error);
        err.protocolError = true;
        p.reject(err);
      } else p.resolve(msg);
    }
  }

  /** Streamed display frames (a long eval emitting plots as it goes). An
   *  unknown event kind is logged and dropped — never an error, so a newer
   *  compiler's events can't break an older client. */
  function handleEvent(msg) {
    if (msg.event !== "display") {
      log(`ignoring unknown event "${msg.event}"`);
      return;
    }
    const res = display.frameFromEvent(msg);
    if (res.ok) display.publish(res.frame, tag);
    else log(res.reason);
  }

  function handleStderr(chunk) {
    // Free-form compiler logging — never parsed, just surfaced.
    const text = String(chunk).trimEnd();
    if (text) log(text);
  }

  /** Spawn the child and wire it up. Per-process closures (rather than
   *  reading the outer `proc`/decoder from the listeners) so a stray event
   *  from an already-torn-down process can never be mistaken for the current
   *  one. */
  function spawnProcess() {
    const exe = deps.findCompiler();
    const child = cp.spawn(exe, spawnArgs(), {
      cwd: spawnCwd(),
      env: spawnEnv(),
      windowsHide: true,
    });
    const decoder = proto.createDecoder();
    proc = child;
    child.stdout.setEncoding("utf8");
    child.stderr.setEncoding("utf8");
    child.stdout.on("data", (chunk) => {
      if (proc !== child) return;
      handleStdout(decoder, chunk);
    });
    child.stderr.on("data", (chunk) => {
      if (proc !== child) return;
      handleStderr(chunk);
    });
    child.on("error", (e) => {
      if (proc !== child) return;
      teardown(`could not run '${exe} ide serve': ${e.message}`, true);
    });
    child.on("exit", (code, signal) => {
      if (proc !== child) return;
      teardown(`blade ide serve exited (code=${code}${signal ? `, signal=${signal}` : ""})`, true);
    });
  }

  /** Send one request, tracked by id, rejecting on `timeoutMs`. On timeout or
   *  a synchronous write failure, tears the process down (see teardown) — a
   *  wedged or broken pipe means every other in-flight request is equally
   *  dead, and the next ensureReady() call will respawn subject to backoff.
   *  An explicit `{"error": "..."}` response, by contrast, means the process
   *  is alive and answering correctly — that just rejects THIS request. */
  function sendRequest(encode, timeoutMs) {
    return new Promise((resolve, reject) => {
      const id = nextId++;
      let settled = false;
      const timer = setTimeout(() => {
        if (settled) return;
        settled = true;
        pending.delete(id);
        const reason = `request ${id} timed out after ${timeoutMs}ms`;
        reject(new Error(`blade ide serve: ${reason}`));
        teardown(reason, true);
      }, timeoutMs);
      pending.set(id, {
        resolve: (msg) => {
          if (settled) return;
          settled = true;
          clearTimeout(timer);
          resolve(msg);
        },
        reject: (err) => {
          if (settled) return;
          settled = true;
          clearTimeout(timer);
          reject(err);
        },
      });
      try {
        if (!proc) throw new Error("no active process");
        proc.stdin.write(encode(id));
      } catch (e) {
        if (settled) return;
        settled = true;
        clearTimeout(timer);
        pending.delete(id);
        reject(new Error(`blade ide serve: could not write request: ${e.message}`));
        teardown(`could not write request: ${e.message}`, true);
      }
    });
  }

  /** Spawn (if needed) and ping-probe the process, sharing one attempt across
   *  concurrent callers. */
  function doHandshake() {
    return (async () => {
      try {
        spawnProcess();
      } catch (e) {
        recordFailure(`could not start 'ide serve': ${e.message}`);
        throw new Error("blade ide serve unavailable");
      }
      let msg;
      try {
        msg = await sendRequest((id) => proto.encodePing(id), PING_TIMEOUT_MS);
      } catch (e) {
        // A timeout or write failure already ran teardown()+recordFailure()
        // above; an explicit {error} response from a live process has not,
        // so cover that one remaining case here without double-counting.
        if (proc) teardown(`ping error: ${e.message}`, true);
        throw new Error("blade ide serve unavailable");
      }
      if (!msg || msg.ok !== true) {
        teardown("ping response missing ok:true", true);
        throw new Error("blade ide serve unavailable");
      }
      recordSuccess();
      log(`available — serve=${msg.serve}, version=${msg.version || "unknown"}`);
    })();
  }

  function ensureReady() {
    if (proc && availability === "yes") return Promise.resolve();
    if (availability === "no") return Promise.reject(new Error("blade ide serve unavailable"));
    if (handshake) return handshake;
    const now = Date.now();
    if (now < nextSpawnAllowedAt) {
      return Promise.reject(
        new Error(`blade ide serve: backing off for ${nextSpawnAllowedAt - now}ms`)
      );
    }
    const p = doHandshake();
    handshake = p;
    // This derived promise exists only to clear `handshake` once p settles.
    // `p` itself is returned below and is what every real caller awaits/
    // catches (check/eval/resetSession) — but a `.finally()`'s OWN derived
    // promise re-rejects when p rejects, and nothing else references it, so
    // without this trailing `.catch()` a failed handshake becomes an
    // unhandled rejection (harmless-ish in an extension host, fatal under
    // plain Node).
    p.finally(() => {
      if (handshake === p) handshake = null;
    }).catch(() => {});
    return p;
  }

  // --- Public surface (this client) -------------------------------------------

  /** Tri-state capability: "unknown" (never probed), "yes", "no" (latched —
   *  see the module header). Synchronous — reflects the LAST probe/request
   *  outcome, does not itself trigger one. */
  function available() {
    return availability;
  }

  /**
   * Check `source` (the live buffer text, not necessarily saved) at `tier`.
   * Lazily spawns and pings on first use. Resolves with the response payload
   * object (diagnostics/bindings/providers/deduced/calls/kernels, "id", and
   * "tier" echoed back — plus "concreteType" entries on "full" responses) or
   * rejects with an Error whose message explains why (unavailable, backing
   * off, timed out, or a protocol-level `{"error": "..."}` response).
   * @param {string} fileName absolute path (resolved provider-relative paths
   *   are the compiler's job — it chdirs to this file's directory per
   *   request)
   * @param {string} source full buffer text
   * @param {"fast"|"full"} tier
   * @param {number} [timeoutMs] default 10s (fast) / 30s (full)
   */
  function check(fileName, source, tier, timeoutMs) {
    const t = tier === "full" ? "full" : "fast";
    const ms = timeoutMs || DEFAULT_TIMEOUT_MS[t];
    return ensureReady().then(() => {
      if (!proc) throw new Error("blade ide serve unavailable");
      return sendRequest((id) => proto.encodeCheck(id, t, fileName, source), ms);
    });
  }

  /**
   * Check a whole notebook at `tier`: `cells` is the ordered source text of
   * every CODE cell, and the COMPILER assembles them into one session source
   * (rebind-in-place, bare-expression wrapping — ReplSession.assembleCells)
   * before checking it. Stateless, exactly like check(): the entire notebook
   * travels in the request, nothing is remembered between them.
   *
   * Resolves with a normal check payload PLUS `windows` — one
   * `{startLine, endLine[, wrapLine, wrapCol]}` per input cell, in input
   * order, naming where that cell's text landed in the assembled source.
   * Rejects like eval(): `err.protocolError === true` means the process
   * answered LIVE but doesn't know "checkCells" (a compiler predating
   * notebook checking); anything else is a transport failure.
   * @param {string} fileName absolute path the compiler chdirs to the
   *   directory of, same role as check()'s
   * @param {string[]} cells code-cell sources in notebook order
   * @param {"fast"|"full"} tier
   * @param {number} [timeoutMs] default 10s (fast) / 30s (full)
   */
  function checkCells(fileName, cells, tier, timeoutMs) {
    const t = tier === "full" ? "full" : "fast";
    const ms = timeoutMs || DEFAULT_TIMEOUT_MS[t];
    return ensureReady().then(() => {
      if (!proc) throw new Error("blade ide serve unavailable");
      return sendRequest((id) => proto.encodeCheckCells(id, t, fileName, cells), ms);
    });
  }

  /**
   * Evaluate `source` as the next submission in REPL session `session`
   * (created on first use; append or rebind-in-place by top-level name —
   * same semantics as one `blade repl` submission). Lazily spawns and pings
   * on first use, exactly like check(). Resolves with the eval response
   * payload (`kept`, `exitCode`, `lane`, `elapsedMs`, `stdout`, `stderr`,
   * `bindings`, `diagnostics`) or rejects with an Error — `err.protocolError
   * === true` means the process answered LIVE but doesn't understand "eval"
   * (a compiler built before notebook support); any other rejection is a
   * transport failure (unavailable, backing off, timed out).
   * @param {string} session session key
   * @param {string} source the cell's source text
   * @param {string} [cwd] absolute directory relative data paths resolve
   *   against (the notebook file's directory); omitted for untitled
   *   notebooks
   * @param {number} [timeoutMs] default 30s — eval may fall back to g++;
   *   callers evaluating notebook cells should pass a generous override
   */
  function evalCode(session, source, cwd, timeoutMs) {
    const ms = timeoutMs || DEFAULT_TIMEOUT_MS.full;
    return ensureReady().then(() => {
      if (!proc) throw new Error("blade ide serve unavailable");
      return sendRequest((id) => proto.encodeEval(id, session, source, cwd), ms);
    });
  }

  /**
   * Discard session `session`'s accumulated bindings server-side (Restart
   * Kernel). Resolves with `{ok:true}` or rejects exactly like eval() —
   * `err.protocolError === true` for "unsupported command" on an old
   * compiler.
   * @param {string} session session key
   * @param {number} [timeoutMs] default 10s — resetSession does no
   *   re-lowering, so this should be near-instant
   */
  function resetSession(session, timeoutMs) {
    const ms = timeoutMs || DEFAULT_TIMEOUT_MS.fast;
    return ensureReady().then(() => {
      if (!proc) throw new Error("blade ide serve unavailable");
      return sendRequest((id) => proto.encodeResetSession(id, session), ms);
    });
  }

  /**
   * Re-render a figure the caller already has as a static image, through the
   * compiler's GR worker. Post-hoc: `args.spec` is the retained figure JSON
   * (`{data, layout}`), nothing re-runs, and the program need not still exist.
   *
   * Resolves with `{id, frame}` — a complete display frame (`image/png` by
   * default, base64) that can go straight through this package's
   * `display.decodeFrame` / `display.publish`. `args.plotId` is echoed into
   * `frame.meta.id`, which is what makes a panel merge the render into the
   * plot it came from rather than appending a new one.
   *
   * Rejects like eval(): `err.protocolError === true` means the process
   * answered LIVE but doesn't know "renderPlot" (a compiler predating GR
   * support — keep the toggle disabled), and that is also how a caller learns
   * GR itself is missing, since an unavailable helper or unset GRDIR comes
   * back as a live `{"error": ...}` naming the reason.
   *
   * @param {{spec: object, plotId?: string, width?: number, height?: number,
   *          format?: "png"|"svg"|"pdf"}} args
   * @param {{timeoutMs?: number}} [opts] default 30s — the worker's first
   *   render pays GR's ~2.6s cold start, later ones are tens of ms
   */
  function renderPlot(args, opts) {
    const ms = (opts && opts.timeoutMs) || DEFAULT_TIMEOUT_MS.full;
    return ensureReady().then(() => {
      if (!proc) throw new Error("blade ide serve unavailable");
      return sendRequest((id) => proto.encodeRenderPlot(id, args), ms);
    });
  }

  /**
   * THE RENDER FAST PATH: recompute an evaluated session under a new camera
   * without re-running it (see serveProto.encodeRender). `bindings` are the
   * camera binding names the figure's layout declared; `values` are the new
   * numbers, positionally.
   *
   * Resolves with `{id, ok, cached, frames, elapsedMs, exitCode, stderr}`.
   * `cached:true` is the steady state and means the executable was reused --
   * the whole reason this command exists. The frames themselves arrive on the
   * display bus as live events, not in the response.
   *
   * Rejects like eval(): `err.protocolError === true` means the process
   * answered LIVE but predates this command, and the caller should fall back
   * to re-running the camera cell.
   *
   * @param {string} session
   * @param {string[]} bindings
   * @param {number[]} values
   * @param {string} [cwd]
   * @param {{timeoutMs?: number}} [opts] default the full tier's -- the FIRST
   *   call compiles, every one after is a re-run
   */
  function render(session, bindings, values, cwd, opts) {
    const ms = (opts && opts.timeoutMs) || DEFAULT_TIMEOUT_MS.full;
    return ensureReady().then(() => {
      if (!proc) throw new Error("blade ide serve unavailable");
      return sendRequest((id) => proto.encodeRender(id, session, bindings, values, cwd), ms);
    });
  }
  /** Tear down the current process (best-effort clean `shutdown` first) and
   *  reset ALL state so the next check()/eval() re-probes from scratch. Safe
   *  to call when nothing is running. Also doubles as this client's "kill and
   *  restart" primitive — an interrupt handler calls this directly to
   *  hard-kill a stuck g++ eval; the shutdown write is best-effort (a busy
   *  single-threaded compiler loop may never read it), the kill() inside
   *  teardown() is what actually guarantees the process dies. */
  function dispose() {
    if (proc) {
      try {
        proc.stdin.write(proto.encodeShutdown());
      } catch (_) {
        /* pipe already gone — the kill() in teardown() below covers it */
      }
    }
    teardown("blade ide serve disposed", false);
    availability = "unknown";
    established = false;
    consecutiveFailures = 0;
    nextSpawnAllowedAt = 0;
    handshake = null;
  }

  return { available, check, checkCells, eval: evalCode, render, resetSession, renderPlot, dispose };
}

module.exports = { createClient };
