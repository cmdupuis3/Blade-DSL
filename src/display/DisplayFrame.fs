// Display frames: the compiler's half of the `{mime, data}` rich-output wire
// format specified in Blade-REPL/docs/display-frames.md. One frame is one
// MIME-typed payload -- a plot, an image -- produced by evaluated Blade code
// and carried to the editor alongside stdout.
//
// This module owns the FORMAT and nothing else: the sentinel bytes, the
// encoding-inference rule, the JSON escape, and the exact byte sequence of one
// frame line. Four consumers share it, which is the whole point of putting it
// this early in the compile order (it depends on nothing but FSharp.Core):
//
//   Interp/Core.fs   evaluates IRDisplayEmit -> `emit` (buffered, flushed by
//                    Interp/Run.fs ahead of the binding prints)
//   CodeGen.fs       emits a C++ `blade_display::emit` MIRROR of `emit`; the
//                    two must agree byte-for-byte or the differential gate
//                    (tests/InterpDiff.fs) fails, which is exactly the pin we
//                    want on this format
//   ReplSession.fs   lifts frame lines out of a run's stdout into EvalResult
//   IdeServe.fs      writes them as the eval response's `display` array
//
// Frames are produced on stdout in BOTH lanes because that is the one place
// the interpreter and the compiled binary already agree byte-for-byte, and
// because the REPL channel (spec section 4) needs them there anyway. The serve
// channel (spec section 2) is then a pure extraction at the session boundary,
// not a second emission path.
module Blade.Display.Frame

open System
open System.Text

/// Envelope version (spec section 1: `v`). A reader rejects anything greater.
[<Literal>]
let Version = 1

/// The REPL channel's line prefix: SOH, `blade-display`, SOH -- 15 bytes.
/// The control-character delimiters are what make the prefix unforgeable: JSON
/// escaping turns a payload's own SOH into `\u0001` (see `escape`), so a frame
/// can never re-open a frame, and no escaping scheme is needed on top.
let Sentinel = "\u0001blade-display\u0001"

/// The LIVE-PLOT STREAM mime. A frame carrying exactly this mime is the one
/// kind an `ide serve` eval forwards WHILE the program runs (the `sink` below)
/// instead of buffering it to end-of-run; every other mime keeps the buffered
/// path unchanged. Frozen by docs/plans/plan-equivariant-nn-notebooks.md
/// section 4 and implemented verbatim on both sides of the wire, so it is a
/// literal here rather than a caller's spelling.
[<Literal>]
let StreamMime = "application/vnd.blade.plotstream.v1+json"

/// Prefix for generated `meta.id`s. Frames with equal ids are alternate
/// renders of the SAME plot, so the panel merges rather than appends -- which
/// is what makes the REPL's re-run-the-whole-session model harmless: replaying
/// a session re-emits every earlier frame with the SAME ordinal, and the panel
/// updates the existing entries in place instead of growing.
///
/// Mutable so a front end holding several independent sessions can keep their
/// ids apart (`ide serve` sets it per notebook). Left at the default, ids are a
/// pure function of the program, which is what the differential gate and the
/// corpus pins require. CodeGen BAKES the current value into the generated C++,
/// so both lanes of one compilation always agree.
let mutable SessionTag = "blade-"

/// A `SessionTag` for one session key. Deterministic (FNV-1a, not
/// String.GetHashCode, which .NET randomizes per process) so the same notebook
/// keeps the same plot identities across restarts of `ide serve`.
let tagForSession (key: string) : string =
    let mutable h = 2166136261u
    for ch in key do
        h <- (h ^^^ uint32 ch) * 16777619u
    sprintf "s%08x-" h

// Per-run emission state. The interpreter runs one program at a time on one
// worker thread (Runtime.runOnLargeStack), so plain mutables are enough; the
// compiled lane's counterpart is a `static int` in the generated C++.
let private buffer = ResizeArray<string>()
let mutable private ordinal = 0
let mutable private sunk = 0

/// Every frame this run has delivered, in order: the line, whether it went to
/// the live sink rather than the buffer, and whether it consumed a run
/// ordinal. Interp/Run.fs brackets each top-level binding with `producedCount`
/// to learn which frames that binding emitted, so a session memo can REPLAY
/// them instead of re-running the binding that computed them.
let private produced = ResizeArray<string * bool * bool>()

/// The LIVE FRAME SINK, installed by `IdeServe`'s eval handler for the length
/// of one evaluation and by nothing else. When it is set, a frame whose mime is
/// exactly `StreamMime` is handed to it AS IT IS PRODUCED and is NOT buffered
/// -- which is what turns a training loop's per-batch plot into something the
/// editor can paint while the loop is still running.
///
/// Scope is deliberately narrow, because the alternative breaks the format's
/// central pin: with no sink (`blade run`, `blade repl`, the corpus, the
/// interpreter/g++ differential gate) EVERY frame stays an ordinary buffered
/// sentinel line, byte-identical between the two lanes. The compiled lane has
/// no sink at all and needs none -- its frames already reach stdout as the
/// program runs.
///
/// A plain mutable is enough: one interpreter run at a time on one worker
/// thread, and `ide serve` handles one request at a time (IdeServe.serveLoop).
let mutable sink : (string -> unit) option = None

/// Install the live frame sink. Always paired with `clearSink` in a `finally`.
let setSink (f: string -> unit) = sink <- Some f

/// Remove the live frame sink; frames go back to the buffer.
let clearSink () = sink <- None

/// Clear the per-run state. Called at the top of every interpreter run so the
/// n-th emission of a program is always id `<tag><n>`, run after run. The sink
/// is NOT touched: it belongs to the caller that installed it (one eval may
/// drive more than one run), and only that caller clears it.
let resetRun () =
    buffer.Clear()
    produced.Clear()
    ordinal <- 0
    sunk <- 0

/// How many frames this run has emitted so far. Interp/Run.fs brackets each
/// top-level binding with it to find the frame EMITTERS: a session memo must
/// never adopt one, because a binding that does not re-run cannot re-emit, and
/// the editor keys its plot panel on the frames of the run it is showing.
/// Frames handed to the sink count too -- they were emitted, they just did not
/// travel by way of stdout, and a streaming binding is exactly the one that
/// must keep re-running.
let emitted () = buffer.Count + sunk

/// Take the frames this run produced, in emission order.
let drain () : string list =
    let xs = List.ofSeq buffer
    buffer.Clear()
    xs

/// How many frames this run has delivered. The bracket Interp/Run.fs takes
/// around each top-level binding; `producedSince` reads the window back.
let producedCount () = produced.Count

/// The frames delivered since `n`, in emission order.
let producedSince (n: int) : (string * bool * bool) list =
    [ for i in n .. produced.Count - 1 -> produced.[i] ]

/// Re-deliver frames a binding produced on an EARLIER run of the same session,
/// without re-running the binding that computed them.
///
/// Sound for the same reason caching the binding's value is: the session memo
/// may only be offered to a run whose prefix is unchanged (see SessionMemo in
/// Interp/Run.fs), so a binding eligible for adoption had the same inputs, and
/// the frames it would emit now are the frames it emitted then. Each frame
/// goes back to the channel it originally took, and one that consumed a run
/// ordinal consumes one again -- so the ids of the frames AROUND it are
/// unchanged, and a replayed run is indistinguishable, frame for frame, from
/// the run that computed it.
let replay (frames: (string * bool * bool) list) : unit =
    for (line, sank, usedOrdinal) in frames do
        if usedOrdinal then ordinal <- ordinal + 1
        match sink with
        | Some f when sank ->
            sunk <- sunk + 1
            f line
        | _ -> buffer.Add line
        produced.Add(line, sank, usedOrdinal)

/// `encoding` implied by a mime type (spec section 1): JSON-shaped mimes carry
/// an inline JSON value, `text/*` carries a string, everything else is binary
/// and travels base64.
let encodingFor (mime: string) : string =
    if mime = "application/json" || mime.EndsWith "+json" then "json"
    elif mime.StartsWith "text/" then "utf8"
    else "base64"

/// Is `data` inserted verbatim (an inline JSON value) or as a quoted, escaped
/// JSON string? Exactly `encoding <> "json"`.
let quotedFor (mime: string) : bool = encodingFor mime <> "json"

/// `type/subtype`, per the spec's mime grammar.
let isMimeType (s: string) : bool =
    let part (p: string) =
        p.Length > 0
        && (Char.IsLetterOrDigit p.[0])
        && p |> Seq.forall (fun c -> Char.IsLetterOrDigit c || c = '.' || c = '+' || c = '_' || c = '-')
    match s.Split('/') with
    | [| a; b |] -> part a && part b
    | _ -> false

/// JSON string escape. Control characters below 0x20 go out as `\u00xx`, which
/// is what disarms a payload containing the sentinel's own SOH. Mirrored
/// character-for-character by the generated C++ (`cppRuntime` below).
let escape (s: string) : string =
    let sb = StringBuilder(s.Length + 8)
    for ch in s do
        match ch with
        | '"' -> sb.Append "\\\"" |> ignore
        | '\\' -> sb.Append "\\\\" |> ignore
        | '\n' -> sb.Append "\\n" |> ignore
        | '\r' -> sb.Append "\\r" |> ignore
        | '\t' -> sb.Append "\\t" |> ignore
        | '\b' -> sb.Append "\\b" |> ignore
        | '\f' -> sb.Append "\\f" |> ignore
        | c when c < ' ' -> sb.AppendFormat("\\u{0:x4}", int c) |> ignore
        | c -> sb.Append c |> ignore
    sb.ToString()

/// `s` as a QUOTED, escaped JSON string -- the value of `display.json_string`,
/// and what stdlib/plot.blade wraps every user title/axis label in. Same escape
/// table as a quoted frame payload, because it is the same problem one level
/// down: a title containing `"` or `\` concatenated raw into a figure object
/// produces JSON no reader accepts. Mirrored by `blade_display::jsonstr`.
let jsonString (s: string) : string = "\"" + escape s + "\""

/// One JSON number: `rendered` when `x` is finite, `"null"` otherwise. JSON has
/// no NaN/Infinity literal -- plotly's own encoder writes `null` for both -- so
/// every element the numeric serializers emit goes through this guard rather
/// than straight out as `nan`/`inf`/`-inf`, which are bare identifiers to a
/// parser. `rendered` is the caller's shortest round-trip rendering: this
/// module compiles before Interp.CppFormat and cannot name it. Mirrored by the
/// `std::isfinite` branch in `blade_display::jsonval`.
let jsonNumber (rendered: string) (x: float) : string =
    if Double.IsFinite x then rendered else "null"

/// The static leading half of a frame, everything up to and including
/// `"data":`. Built at ELABORATION time (the mime is a literal, see
/// DisplayElaborate) so neither runtime lane has to know the encoding rule.
let headFor (mime: string) : string =
    $"{{\"v\":{Version},\"mime\":\"{mime}\",\"encoding\":\"{encodingFor mime}\",\"data\":"

/// A user `meta` object literal reduced to the tail that follows the generated
/// `"id"` entry: `{"title":"x"}` -> `,"title":"x"`, `{}` -> `""`. The braces
/// come off because `id` is generated at runtime and has to lead the object.
/// Returns None when the text is not brace-delimited (the elaborator turns
/// that into a user-facing error).
let metaTailOf (metaJson: string) : string option =
    let t = metaJson.Trim()
    if t.Length >= 2 && t.StartsWith "{" && t.EndsWith "}" then
        let inner = t.Substring(1, t.Length - 2).Trim()
        Some (if inner = "" then "" else "," + inner)
    else None

/// One frame line, without its terminating newline, with the `meta.id` text
/// already decided. The two public composers differ in that ONE substring and
/// in nothing else, which is the property the byte pins are here to keep.
let private composeWith (head: string) (quoted: bool) (data: string) (metaTail: string) (idText: string) : string =
    let payload = if quoted then "\"" + escape data + "\"" else data
    Sentinel + head + payload + ",\"meta\":{\"id\":\"" + idText + "\"" + metaTail + "}}"

/// One frame line, without its terminating newline. `head` and `metaTail` are
/// the elaboration-time constants; `data` is the runtime payload; `ord` is this
/// run's 1-based emission ordinal.
let composeLine (head: string) (quoted: bool) (data: string) (metaTail: string) (ord: int) : string =
    composeWith head quoted data metaTail (SessionTag + string ord)

/// `composeLine`'s twin for `display.emit_id`: the `meta.id` is a RUNTIME
/// string rather than `<SessionTag><ordinal>`, escaped exactly like any other
/// JSON string value (a channel name with a quote in it must not be able to
/// open a key of its own). Everything else about the line -- head, payload
/// quoting, meta tail, the closing braces -- is the same bytes `composeLine`
/// produces, because it is the same code.
let composeLineId (head: string) (quoted: bool) (data: string) (metaTail: string) (id: string) : string =
    composeWith head quoted data metaTail (escape id)

/// The head of a stream frame. Head equality IS mime equality: `headFor`
/// embeds the mime verbatim, so comparing the whole head is the exact test
/// "this frame's mime is `StreamMime`" without re-parsing the head.
let private streamHead = headFor StreamMime

/// Route one composed line: to the live sink when one is installed AND this
/// frame is LIVE, otherwise to the run buffer. Live means the frame carries a
/// stable identity: a stream chunk, or any `display.emit_id` frame (`hasId`,
/// set by emitId below) -- a stable-id figure is re-emittable and a viewer
/// merges it by `meta.id`, so forwarding it the moment it is produced is what
/// lets a long cell ANIMATE a plot (a continuous zoom dive, a per-step field)
/// and lets a recomputed view repaint before its eval settles. Ordinal
/// (`blade-N`) frames stay buffered: their ids are per-run bookkeeping, and
/// they are results, not installments. The no-sink path is the ONLY path a
/// `blade run` / corpus / differential-gate program can take, and it is the
/// pre-sink code verbatim.
let private deliver (hasId: bool) (head: string) (line: string) : bool =
    let sank =
        match sink with
        | Some f when hasId || head = streamHead ->
            sunk <- sunk + 1
            f line
            true
        | _ ->
            buffer.Add line
            false
    // `emit` consumes an ordinal and `emitId` does not, which is exactly
    // `hasId` -- recorded so a replay reproduces the same id sequence.
    produced.Add(line, sank, not hasId)
    true

/// Interpreter-lane emission: buffer one frame and answer `true` (the value
/// `display.emit` evaluates to). Buffered rather than written because the
/// interpreter builds its whole stdout after every binding has been evaluated
/// -- Interp/Run.fs writes these ahead of the binding prints, which is where
/// the compiled binary's `std::cout` writes land too (they run inside main()'s
/// body, before the timing line and the print block).
let emit (head: string) (quoted: bool) (data: string) (metaTail: string) : bool =
    ordinal <- ordinal + 1
    deliver false head (composeLine head quoted data metaTail ordinal)

/// Interpreter-lane emission with a CALLER-CHOSEN `meta.id` (`display.emit_id`).
///
/// The run ordinal is deliberately NOT consumed: an explicit id is already
/// stable across calls and across a session replay, and leaving the counter
/// alone means adding a streaming plot to a notebook cannot renumber the
/// `blade-N` ids of the ordinary `display.emit` frames around it. The generated
/// C++ (`blade_display::emit_id`) leaves its own counter alone for the same
/// reason, which is what keeps the two lanes byte-identical.
let emitId (head: string) (quoted: bool) (data: string) (metaTail: string) (id: string) : bool =
    deliver true head (composeLineId head quoted data metaTail id)

/// Split a program's raw stdout into the text a terminal should show and the
/// frame JSON strings it carried. Frame lines are always whole lines at column
/// 0 (nothing else writes to stdout while a program body runs), so this is a
/// line filter, not a parser. Used by the `ide serve` lane to fill the eval
/// response's `display` array; the interactive REPL leaves stdout alone and
/// lets the extension do its own extraction.
let extract (stdout: string) : string * string list =
    if String.IsNullOrEmpty stdout || not (stdout.Contains Sentinel) then (stdout, [])
    else
        let frames = ResizeArray<string>()
        let kept = ResizeArray<string>()
        for raw in stdout.Split('\n') do
            let line = if raw.EndsWith "\r" then raw.Substring(0, raw.Length - 1) else raw
            if line.StartsWith Sentinel then frames.Add(line.Substring Sentinel.Length)
            else kept.Add raw
        (String.Join("\n", kept), List.ofSeq frames)

/// The generated C++ mirror of `escape`/`composeLine`/`emit`, emitted into
/// every program's preamble (header-only, `static inline`, and free when
/// unused, so it needs no per-program feature scan). `\x01` sits in its OWN
/// string literal because `"\x01blade-display"` would parse `\x01b` as one
/// oversized hex escape -- adjacent-literal concatenation is the fix, not a
/// style choice.
///
/// BYTE PARITY WITH `emit` IS THE CONTRACT: the differential gate runs the same
/// program down both lanes and compares stdout, so any divergence here is a
/// test failure by construction.
let cppRuntime () : string list =
    [ "#include <sstream>  // blade_display::json1/json2/jsonnum"
      "#include <cmath>    // blade_display::jsonval (non-finite -> null)"
      "#include <charconv> // blade_display::jsonfloat (shortest round-trip)"
      "#include <cstdlib>"
      "#include <string>"
      "#include <type_traits>"
      "namespace blade_display {"
      "// Display frames (docs/display-frames.md). Mirrors Blade.Display.Frame."
      "static int __blade_display_ord = 0;"
      "static inline std::string __blade_display_esc(const std::string& s) {"
      "    static const char* hx = \"0123456789abcdef\";"
      "    std::string o; o.reserve(s.size() + 8);"
      "    for (std::size_t i = 0; i < s.size(); ++i) {"
      "        unsigned char c = (unsigned char)s[i];"
      "        switch (c) {"
      "        case '\"': o += \"\\\\\\\"\"; break;"
      "        case '\\\\': o += \"\\\\\\\\\"; break;"
      "        case '\\n': o += \"\\\\n\"; break;"
      "        case '\\r': o += \"\\\\r\"; break;"
      "        case '\\t': o += \"\\\\t\"; break;"
      "        case '\\b': o += \"\\\\b\"; break;"
      "        case '\\f': o += \"\\\\f\"; break;"
      "        default:"
      "            if (c < 0x20) { o += \"\\\\u00\"; o += hx[(c >> 4) & 0xF]; o += hx[c & 0xF]; }"
      "            else o += (char)c;"
      "        }"
      "    }"
      "    return o;"
      "}"
      "// A user string as a quoted, escaped JSON string (display.json_string)."
      "// Mirrors Blade.Display.Frame.jsonString."
      "static inline std::string jsonstr(const std::string& s) {"
      "    return \"\\\"\" + __blade_display_esc(s) + \"\\\"\";"
      "}"
      "// JSON serialization of numeric arrays / scalars (display.json_array /"
      "// display.json_num). A floating element goes out as its SHORTEST"
      "// round-trip rendering (jsonfloat below) -- exact, where the print"
      "// block's setprecision(15) would quantize a Float64 to 15 digits;"
      "// Blade.Interp.CppFormat.formatFloatShortest is the byte-exact mirror,"
      "// so the differential gate pins the two lanes together."
      "//"
      "// jsonval is the per-element guard: JSON has no NaN/Infinity literal, so"
      "// a non-finite value goes out as `null` (what plotly's own encoder"
      "// writes) instead of the stream's bare `nan`/`inf`/`-inf`. Integral T"
      "// always takes the finite branch. Mirrors Frame.jsonNumber -- and the"
      "// test is on the VALUE, not on the rendered text, because the spelling"
      "// of a non-finite is implementation-defined (`nan`, `-nan`, `NaN`,"
      "// `1.#QNAN` across the standard libraries). isfinite is the portable"
      "// predicate; matching the formatter's output would not be."
      "// Shortest round-trip rendering of a double, laid out like \"%.17g\": the"
      "// fewest significant digits that parse back to the same double, fixed"
      "// notation for decimal exponents in [-4, 17), scientific otherwise."
      "// Mirrors CppFormat.formatFloatShortest / assembleShortest byte for byte."
      "static inline std::string jsonfloat(double v) {"
      "    char buf[64];"
      "    auto r = std::to_chars(buf, buf + sizeof buf, v, std::chars_format::scientific);"
      "    std::string s(buf, r.ptr);                     // [-]d[.ddd]e[+-]dd"
      "    std::string sign; size_t p = 0;"
      "    if (s[0] == '-') { sign = \"-\"; p = 1; }"
      "    size_t e = s.find('e');"
      "    std::string digits;"
      "    for (size_t i = p; i < e; ++i) if (s[i] != '.') digits += s[i];"
      "    while (digits.size() > 1 && digits.back() == '0') digits.pop_back();"
      "    if (digits == \"0\") return sign + \"0\";"
      "    int x = std::atoi(s.c_str() + e + 1);"
      "    std::string out = sign;"
      "    if (x < -4 || x >= 17) {"
      "        out += digits[0];"
      "        if (digits.size() > 1) { out += '.'; out += digits.substr(1); }"
      "        out += 'e'; out += (x < 0 ? '-' : '+');"
      "        int a = x < 0 ? -x : x;"
      "        if (a < 10) out += '0';"
      "        out += std::to_string(a);"
      "    } else if (x >= 0) {"
      "        size_t intLen = (size_t)x + 1;"
      "        std::string padded = digits;"
      "        if (padded.size() < intLen) padded.append(intLen - padded.size(), '0');"
      "        out += padded.substr(0, intLen);"
      "        if (padded.size() > intLen) { out += '.'; out += padded.substr(intLen); }"
      "    } else {"
      "        out += \"0.\"; out.append((size_t)(-x) - 1, '0'); out += digits;"
      "    }"
      "    return out;"
      "}"
      "template <typename T>"
      "static inline void jsonval(std::ostringstream& o, const T& x) {"
      "    if (!std::isfinite((double)x)) { o << \"null\"; return; }"
      "    if constexpr (std::is_floating_point_v<T>) o << jsonfloat((double)x); else o << x;"
      "}"
      "template <typename A>"
      "static inline std::string json1(const A& a) {"
      "    std::ostringstream o; o << '[';"
      "    for (size_t i = 0; i < a.extents[0]; ++i) { if (i) o << ','; jsonval(o, a[i]); }"
      "    o << ']'; return o.str();"
      "}"
      "template <typename A>"
      "static inline std::string json2(const A& a) {"
      "    std::ostringstream o; o << '[';"
      "    for (size_t i = 0; i < a.extents[0]; ++i) {"
      "        if (i) o << ','; o << '[';"
      "        for (size_t j = 0; j < a.extents[1]; ++j) { if (j) o << ','; jsonval(o, a[i][j]); }"
      "        o << ']';"
      "    }"
      "    o << ']'; return o.str();"
      "}"
      "template <typename T>"
      "static inline std::string jsonnum(T x) {"
      "    std::ostringstream o; jsonval(o, x); return o.str();"
      "}"
      "static inline bool emit(const char* head, bool quoted, const std::string& data,"
      "                        const char* metaTail, const char* tag) {"
      "    std::cout << \"\\x01\" \"blade-display\" \"\\x01\" << head;"
      "    if (quoted) std::cout << '\"' << __blade_display_esc(data) << '\"';"
      "    else std::cout << data;"
      "    std::cout << \",\\\"meta\\\":{\\\"id\\\":\\\"\" << tag << ++__blade_display_ord"
      "              << \"\\\"\" << metaTail << \"}}\" << \"\\n\";"
      "    return true;"
      "}"
      "// display.emit_id: the same line with a RUNTIME meta.id in place of"
      "// <tag><ordinal>, escaped like any other JSON string value. The ordinal"
      "// counter is deliberately untouched -- Blade.Display.Frame.emitId does"
      "// not consume one either, so an emit_id call never renumbers the plain"
      "// emit frames around it in EITHER lane."
      "static inline bool emit_id(const char* head, bool quoted, const std::string& data,"
      "                           const char* metaTail, const std::string& id) {"
      "    std::cout << \"\\x01\" \"blade-display\" \"\\x01\" << head;"
      "    if (quoted) std::cout << '\"' << __blade_display_esc(data) << '\"';"
      "    else std::cout << data;"
      "    std::cout << \",\\\"meta\\\":{\\\"id\\\":\\\"\" << __blade_display_esc(id)"
      "              << \"\\\"\" << metaTail << \"}}\" << \"\\n\";"
      "    return true;"
      "}"
      "}" ]
