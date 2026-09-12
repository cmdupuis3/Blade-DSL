// Public types for @blade-lang/ide-protocol.
//
// Hand-written documentation of hand-written JavaScript — there is no build
// step and no checked-in .js.map. If you change a module's behavior, change
// its transcription here in the same edit.

export * from "./check";
export * from "./serve";
export * from "./surface";
export * from "./doctor";

import {
  CheckPayload,
  CellWindow,
} from "./check";
import {
  CheckResponse,
  CheckCellsResponse,
  DisplayEvent,
  DisplayFrame,
  ErrorResponse,
  EvalResponse,
  OkResponse,
  RenderPlotRequest,
  RenderPlotResponse,
  ServeResponse,
  Tier,
} from "./serve";
import { SurfaceJson } from "./surface";

// --- serveProto: NDJSON framing ---------------------------------------------

/** A stateful line decoder for ONE child process's stdout. Feed it chunks as
 *  they arrive; get back the complete messages they contained (zero, one, or
 *  several — fast consecutive responses coalesce into a single `data` event).
 *  Tolerates CRLF. The trailing partial line is retained across calls. */
export interface Decoder {
  push(chunk: string): ServeResponse[];
}

/** The `render` response: one camera change's OUTCOME, not its pictures --
 *  those arrive as live display events while the executable runs. */
export interface RenderResponse {
  id: number;
  ok: boolean;
  /** The executable was reused rather than rebuilt. The steady state, and
   *  the whole reason this command exists; false only on the first call of
   *  a session (or after an edit changed the program around the camera). */
  cached: boolean;
  /** How many display frames the run emitted, all already published. */
  frames: number;
  elapsedMs: number;
  exitCode: number;
  stderr: string;
}

/** The `renderPlot` request minus its envelope — everything the caller
 *  chooses, in one object (see ./serve.d.ts for the field rules). */
export type RenderPlotArgs = Omit<RenderPlotRequest, "id" | "cmd">;

/** Encoders return one complete request LINE, newline included. */
export interface ServeProto {
  encodeCheck(id: number, tier: Tier, file: string, source: string): string;
  encodeCheckCells(id: number, tier: Tier, file: string, cells: string[]): string;
  encodePing(id: number): string;
  encodeEval(id: number, session: string, source: string, cwd?: string): string;
  encodeResetSession(id: number, session: string): string;
  /** Takes its arguments as one object rather than positionally: four of the
   *  five request fields are optional. */
  encodeRenderPlot(id: number, args: RenderPlotArgs): string;
  /** No id, no response. */
  encodeShutdown(): string;
  /** Parse one already-newline-stripped line. A line that isn't valid JSON, or
   *  is valid JSON but not an object, decodes to `{id: null, error}` — shaped
   *  like the protocol's own error responses — rather than throwing. */
  decodeLine(line: string): ServeResponse;
  createDecoder(): Decoder;
}

export declare const serveProto: ServeProto;

// --- replProto: the terminal-shaped `blade repl` ----------------------------

/** Framing for the INTERACTIVE repl, which is a prompt/echo protocol rather
 *  than a data pipe: the child writes a prompt with no trailing newline
 *  whenever it is ready to read, so "stdout ends with PROMPT" is the frame
 *  terminator. */
export interface ReplProto {
  /** "blade> " — the top-level prompt. */
  readonly PROMPT: string;
  /** "  ... " — the continuation prompt, one per line read inside a :paste. */
  readonly CONT: string;
  readonly FALLBACK_NOTICE: string;
  /** Encode one editor submission. Always :paste-framed, even single lines, so
   *  a snippet with unbalanced brackets is rejected instead of leaving the
   *  child at a continuation prompt forever. */
  wireFor(code: string): string;
  frameDone(buf: string): boolean;
  /** A completed frame's stdout minus the CONT prompt prefix. The caller must
   *  already have sliced the trailing PROMPT off. */
  cleanFrame(frame: string): string;
  /** stderr with commentary (the g++ fallback notice, positionless warnings)
   *  removed. */
  significantErr(err: string): string;
  isErrorFrame(out: string, err: string): boolean;
  /** Did the interpreter punt this input to the g++ lane? */
  fellBack(err: string): boolean;
  /** One-line summary for inline display. */
  summarize(out: string, err: string): string;
  ellipsize(s: string, max: number): string;
}

export declare const replProto: ReplProto;

// --- display: frame parsing and the routing hub -----------------------------

/** A frame after validation: `encoding` and `meta` are always populated. */
export interface DecodedFrame extends DisplayFrame {
  v: number;
  encoding: "json" | "utf8" | "base64";
  meta: Record<string, unknown>;
}

/** Decoding never throws — it reports a reason instead, and the caller keeps
 *  the payload as plain text. A plotting bug must not cost a session. */
export type DecodeResult =
  | { ok: true; frame: DecodedFrame }
  | { ok: false; reason: string };

export interface ScanResult {
  text: string;
  frames: DecodedFrame[];
  errors: string[];
}

export interface StreamScanner {
  push(chunk: string): ScanResult;
  /** Give up on a withheld partial line (session end) and emit it as text. */
  flush(): string;
}

export interface Disposable {
  dispose(): void;
}

/**
 * The display-frame parser, shared by every channel that can carry a frame,
 * plus a one-hub publish/subscribe for routing them.
 *
 * The hub is MODULE-LEVEL state: publish and subscribe only connect when every
 * participant requires the SAME module instance. A consumer that keeps a
 * private copy of this file alongside the package silently splits the channel.
 */
export interface Display {
  readonly FRAME_VERSION: number;
  /** The REPL channel's SOH-delimited line prefix. */
  readonly SENTINEL: string;
  readonly MAX_FRAME_CHARS: number;
  readonly PLOTLY_MIME: string;
  readonly PNG_MIME: string;
  defaultEncodingFor(mime: string): "json" | "utf8" | "base64";
  /** Which panel backend produced (or can render) a frame. */
  backendFor(frame: DisplayFrame): string;
  decodeFrame(obj: unknown): DecodeResult;
  parseFrameJson(text: string): DecodeResult;
  encodeReplLine(frame: DisplayFrame): string;
  /** Split a completed REPL stdout frame into terminal text and the frames it
   *  carried. A sentinel line that fails to decode is NOT dropped — its text
   *  stays in `text` and the reason lands in `errors`. */
  scanReplOutput(text: string): ScanResult;
  createStreamScanner(): StreamScanner;
  /** Frames carried by one eval response's `display` array. A response without
   *  the field yields nothing and no error. */
  framesFromEval(resp: unknown): { frames: DecodedFrame[]; errors: string[] };
  /** Is this NDJSON message an out-of-band event rather than a response?
   *  Check this BEFORE any id lookup. */
  isEvent(msg: unknown): msg is DisplayEvent;
  frameFromEvent(msg: unknown): DecodeResult;
  /** Route rejection reasons somewhere. Unset by default, so the module stays
   *  usable from a plain Node script. */
  setLogger(fn: ((line: string) => void) | null): void;
  subscribe(fn: (frame: DecodedFrame, origin: string) => void): Disposable;
  /** Deliver one frame to every subscriber. A throwing subscriber is logged
   *  and skipped. */
  publish(frame: DecodedFrame, origin?: string): void;
  route<T extends { frames?: DecodedFrame[]; errors?: string[] }>(result: T, origin: string): T;
  ingestReplText(text: string, origin?: string): string;
}

export declare const display: Display;

// --- client -----------------------------------------------------------------

/** An error raised because the compiler answered LIVE with `{"error": ...}` —
 *  "I don't understand this request", e.g. a compiler predating a verb. It is
 *  NOT a transport failure: the process is fine, nothing was torn down, and
 *  only this one request was rejected. Callers use it to stop retrying a
 *  command the compiler will never support. */
export interface ProtocolError extends Error {
  protocolError: true;
}

export interface ClientDeps {
  /** REQUIRED. Returns the compiler to spawn. Called at EVERY spawn, so a
   *  client re-resolves after a rebuild — pass a function, not a cached
   *  string, or you lose resolveCompiler's newest-mtime rule. */
  findCompiler(): string;
  /** Where this client's log lines go. Omitted means silence. */
  output?: { appendLine(line: string): void };
  /** Working directory for the child, as a string or a function re-read per
   *  spawn. Undefined inherits the parent's. The compiler also chdirs per
   *  request, so this is only the initial value. */
  cwd?: string | (() => string | undefined);
  /** Spawn argv. Defaults to ["ide", "serve"]; override to point at a fake
   *  server in tests. */
  args?: string[];
  /** The child's environment, as an object or a function re-read per spawn.
   *  Undefined inherits this process's, which is what every caller got before
   *  this hook existed.
   *
   *  It REPLACES rather than extends (Node's spawn semantics), so add to the
   *  parent's rather than passing a bare object:
   *  `env: () => ({ ...process.env, GRDIR: grRoot, PATH: grBin + path.delimiter + process.env.PATH })`.
   *  That is exactly what a host must supply for `renderPlot` to work — the
   *  compiler's GR worker resolves its DLLs off GRDIR/PATH, and both failure
   *  modes are silent crashes. */
  env?: Record<string, string | undefined> | (() => Record<string, string | undefined> | undefined);
}

/** Tri-state capability latch. "unknown" until the first ping resolves or
 *  fails; then "yes", or "no" — and "no" is LATCHED for the life of the
 *  client, so a compiler without the subcommand is not hammered. */
export type Availability = "unknown" | "yes" | "no";

export interface BladeClient {
  /** Synchronous: reflects the last probe/request outcome, and does not itself
   *  trigger one. */
  available(): Availability;
  /** Lazily spawns and pings on first use. Rejects with a plain Error for
   *  transport failure (unavailable, backing off, timed out) or a
   *  ProtocolError for an `{"error": ...}` response. */
  check(fileName: string, source: string, tier: Tier, timeoutMs?: number): Promise<CheckResponse>;
  checkCells(
    fileName: string,
    cells: string[],
    tier: Tier,
    timeoutMs?: number
  ): Promise<CheckCellsResponse>;
  /** Default timeout 30s — eval may fall back to g++, so pass a generous
   *  override for anything real. */
  eval(session: string, source: string, cwd?: string, timeoutMs?: number): Promise<EvalResponse>;
  resetSession(session: string, timeoutMs?: number): Promise<OkResponse>;
  /** THE RENDER FAST PATH: recompute an already-evaluated session under a
   *  new camera WITHOUT re-running it. The compiler builds the program once
   *  with `bindings` erased into a run-time read, then re-runs that
   *  executable per call with only `values` changed -- about 400ms a frame
   *  against seconds for a session evaluation. `bindings` are the camera
   *  names the figure's layout declared; `values` are positional.
   *
   *  The camera stays in the caller's cell -- rewrite it as before, so the
   *  notebook keeps saying where the lens points; the compiled program
   *  simply never sees a literal, which is what lets the binary be reused.
   *
   *  Frames arrive on the display bus as live events, NOT in the response.
   *  Rejects with `protocolError` on a compiler predating the command --
   *  fall back to re-running the camera cell. */
  render(session: string, bindings: string[], values: number[], cwd?: string,
         opts?: { timeoutMs?: number }): Promise<RenderResponse>;
  /** Re-render a retained figure spec as a static image through the
   *  compiler's GR worker. Resolves with a complete display frame, ready for
   *  `display.decodeFrame`/`display.publish`. A ProtocolError means either a
   *  compiler predating the verb or a live compiler that cannot reach GR — the
   *  message says which. Default timeout 30s (the worker's FIRST render pays
   *  GR's ~2.6s cold start; later ones are tens of ms). */
  renderPlot(args: RenderPlotArgs, opts?: { timeoutMs?: number }): Promise<RenderPlotResponse>;
  /** Best-effort clean shutdown, then kill; resets ALL state so the next call
   *  re-probes from scratch. Safe when nothing is running, and doubles as the
   *  "kill a stuck eval" primitive. */
  dispose(): void;
}

/**
 * Build one `ide serve` client — one child process's worth of private state
 * (spawn, ping probe, id correlation, timeouts, backoff, dispose). Two clients
 * never share a process or any other mutable state, so a caller that needs an
 * independent process (a notebook, whose eval can invoke a multi-second g++
 * lane) simply calls this again.
 *
 * `label` (default "blade serve") tags this client's lines in the log.
 */
export declare function createClient(deps: ClientDeps, label?: string): BladeClient;

// --- compiler discovery ------------------------------------------------------

export interface CompilerResolution {
  /** A path, or the bare name "Blade" to be found on PATH. */
  exe: string;
  /** Which rule produced it. "path" means nothing local was found — a script
   *  that should SKIP rather than fail keys off exactly this. */
  origin: "explicit" | "env" | "candidate" | "path";
}

export interface ResolveCompilerOptions {
  /** A path the caller was told to use (a setting, a --compiler flag). */
  explicitPath?: string;
  /** Environment to read; defaults to process.env. The variable consulted is
   *  BLADE_EXE. */
  env?: Record<string, string | undefined>;
  /** Defaults to DEFAULT_CANDIDATES. */
  candidates?: string[];
}

/** Never throws and never spawns: the result is a path, and only an actual
 *  spawn can tell you whether it runs. */
export declare function resolveCompiler(options?: ResolveCompilerOptions): CompilerResolution;

export interface ResolveRepoRootOptions {
  /** Usually resolveCompiler().exe. */
  exe?: string;
  /** Environment to read; defaults to process.env. Consults BLADE_REPO. */
  env?: Record<string, string | undefined>;
}

/** Find the Blade source checkout, if there is one — for repo-only assets
 *  (docs/, examples/). Never required: everything deployed beside the binary
 *  (tests/corpus, stdlib) is reachable without it, and callers MUST degrade
 *  gracefully when this returns undefined. */
export declare function resolveRepoRoot(options?: ResolveRepoRootOptions): string | undefined;

/** The in-repo build outputs, resolved relative to this package rather than
 *  hardcoded: `<repo>/bin/Release/net10.0/Blade.exe` and its Debug twin.
 *  Installed as a dependency these cannot exist, and resolution correctly
 *  falls through. */
export declare const DEFAULT_CANDIDATES: string[];

// --- generated / authored data ----------------------------------------------

/** One code's entry in the knowledge base. */
export interface DiagnosticsKbEntry {
  /** Byte-matches the compiler's own registry title. */
  title: string;
  explanation: string;
  fix: string;
  /** Repo-relative corpus paths, preferring files with the matching
   *  `// ERROR: BLxxxx` pin. tests/corpus ships beside the binary, so these
   *  resolve under both roots. May be empty. */
  examples: string[];
  /** Repo-relative doc paths. Repo-only — absent from a deployed tree. May be
   *  empty. */
  docs: string[];
}

/** protocol/data/diagnostics.json — hand-authored prose keyed by BLxxxx code.
 *  Purely data: never compiled into the binary. */
export interface DiagnosticsKb {
  version: 1;
  codes: Record<string, DiagnosticsKbEntry>;
}

/** surface.json, loaded LAZILY — accessing this throws with instructions when
 *  the file has not been generated. */
export declare const surface: SurfaceJson;

/** data/diagnostics.json, loaded LAZILY — accessing this throws when the
 *  knowledge base is absent, so consumers should either guard the access or
 *  degrade to `surface.diagnostics` for titles. */
export declare const diagnosticsKb: DiagnosticsKb;

export { CheckPayload, CellWindow, DisplayFrame, ErrorResponse, SurfaceJson };
