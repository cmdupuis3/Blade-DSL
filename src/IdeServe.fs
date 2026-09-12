// IdeServe.fs: `blade ide serve` -- the persistent twin of `ide check --json`.
//
// The editor used to re-spawn the compiler on every save; as-you-type checking
// cannot afford that, so this is one long-lived process speaking NDJSON over
// stdin/stdout (one JSON object per line, in both directions):
//
//   -> {"id":N,"cmd":"check","tier":"fast"|"full","file":"<abs path>","source":"<buffer>"}
//   <- the ide check payload, plus top-level "id" and "tier"
//   -> {"id":N,"cmd":"ping"}     <- {"id":N,"ok":true,"serve":1,"version":"..."}
//   -> {"cmd":"shutdown"}        (no response; also exits cleanly on stdin EOF)
//   <- {"id":N|null,"error":"<message>"} for anything malformed
//
// ...plus the NOTEBOOK lane, which evaluates cells with REPL semantics:
//
//   -> {"id":N,"cmd":"eval","session":"<key>","source":"<cell>","cwd":"<dir>"?}
//   <- {"id":N,"kept":B,"exitCode":E,"lane":"interp"|"gpp","elapsedMs":M,
//       "stdout":"..","stderr":"..","bindings":[{name,type,value}],
//       "diagnostics":[{severity,line,col,endLine,endCol,message,code?}],
//       "display":[{v,mime,encoding,data,meta}]}
//
// `display` (Blade-REPL/docs/display-frames.md section 2) carries the rich
// MIME outputs -- plots -- this submission produced, in production order. The
// field is OMITTED when the submission produced none, which is what keeps
// every pre-display-frames client and every non-plotting program on exactly
// the bytes they had before.
//   -> {"id":N,"cmd":"resetSession","session":"<key>"}   <- {"id":N,"ok":true}
//
// ...and the RENDER FAST PATH, which recomputes an evaluated session under a
// new camera without re-running it (see the block above serveLoop):
//
//   -> {"id":N,"cmd":"render","session":"<key>","bindings":["cam_cx",..],
//       "values":[-0.74,..],"cwd":"<dir>"?}
//   <- {"event":"display","id":N,"frame":{...}}   (one per CHANGED frame, live)
//   <- {"id":N,"ok":B,"cached":B,"frames":F,"unchanged":U,"elapsedMs":M,
//       "exitCode":E,"stderr":"..."}
//
// `cached:true` means the executable was reused -- the steady state, and the
// whole reason the lane exists. An older compiler answers `unknown cmd
// 'render'`, which is how a client probes for it.
//
// Only frames whose BYTES MOVED since this session's last render are sent;
// `unchanged` counts the rest. A camera change can only alter the plots that
// read the camera, but the program emits every plot it has, so re-sending all
// of them replays the whole notebook into the panel -- a fixed tour under a
// stable id animates its entire descent again on every zoom.
//
// ...and the LANGUAGE SURFACE, the one command that reads no program at all:
//
//   -> {"id":N,"cmd":"surface"}
//   <- {"id":N,"version":1,"compilerVersion":"..","keywords":[{"word","token"}],
//       "operators":[..],"mathIntrinsics":{"unary","binary","ternary","complex"},
//       "builtins":[..],"scalarTypes":[..],"builtinCalls":[..],
//       "diagnostics":[{"code","title","phase"}]}
//
// Byte-identical to `blade ide surface` apart from the leading `id`. A
// compiler predating this arm answers `unknown cmd 'surface'`, and THAT is how
// a client probes for it -- one more reason the unknown-cmd arm is contract.
//
// ...and the GR RENDER lane, which reads no program either -- it re-renders a
// spec the panel already holds (Blade-REPL/docs/gr-graphics-plan.md section 4.2):
//
//   -> {"id":N,"cmd":"renderPlot","spec":{<figure>},"plotId":"<meta.id>"?,
//       "width":W?,"height":H?,"format":"png"|"svg"|"pdf"?}
//   <- {"id":N,"frame":{"v":1,"mime":"image/png","encoding":"base64",
//                       "data":"<base64>","meta":{"id":"<plotId>","backend":"gr"}}}
//
// The response is a complete DISPLAY FRAME, so the client reuses its existing
// decode/publish path and the panel's merge-by-`meta.id` attaches the render to
// the plot the spec came from -- with the id PINNED FROM THE REQUEST, which is
// what sidesteps the per-emit ordinal entirely. Failures answer with the
// ordinary error shape, and a compiler predating this arm answers
// `unknown cmd 'renderPlot'` (the capability probe again), so a client that
// cannot render simply keeps its GR toggle disabled.
//
// ...and the notebook's SQUIGGLE clock, which typechecks every cell at once:
//
//   -> {"id":N,"cmd":"checkCells","file":"<abs path>","cells":["<cell 0>",..],
//       "tier":"fast"|"full"}
//   <- the ide check payload, plus "id", "tier", and
//      "windows":[{"startLine":S,"endLine":E,"wrapLine":L?,"wrapCol":C?}] --
//      one entry per input cell, in input order
//
// `checkCells` is the one command with no `session`: it assembles the given
// cells into a single source with Blade.ReplSession.assembleCells -- the SAME
// classification and rebind placement `eval` uses, so the squiggles cannot
// disagree with the kernel that will later run those cells -- checks it as one
// file, and reports the 1-based line range each cell landed on so the client
// can rebase the payload onto its own cells. It commits nothing and touches no
// session, so it may interleave with `eval` freely. The client owns which cells
// are code (markdown never reaches here) and holds no assembly rules of its own.
//
// One Blade.ReplSession per `session` key -- a notebook's cells accumulate,
// two notebooks never see each other, and every session dies with the process.
// Diagnostic coordinates are CELL-LOCAL: the compiler reports against the
// assembled session file and the engine rebases them onto the submission.
// An UNKNOWN cmd still answers `{"id":N,"error":...}`, which is how an old
// extension probes a new compiler and vice versa -- so that arm is contract,
// not just courtesy.
//
// stderr stays free-form logging: the client ignores it.
//
// The two tiers are where the pipeline STOPS. Fast = parse + typecheck +
// deduce, exactly what `ide check` has always run -- the editor's squiggle
// clock. Full = fast plus lowering/monomorphization, which is the only stage
// that resolves an HM-polymorphic value binding's `T` down to a concrete type,
// so it feeds `concreteType` upgrades on a slower clock.
//
// Compiles after Lowering.fs (the full tier calls it) and Ide.fs (whose
// payload it wraps), and before Cli.fs, which dispatches `ide serve`.
module Blade.IdeServe

open System
open System.IO
open System.Text
open System.Text.Json

// Full tier

/// Top-level binding name -> its type as monomorphization left it, rendered
/// with Ide.fs's own printers so a `concreteType` never disagrees in spelling
/// with the `type` beside it. A FRESH abstract renderer per binding: schemes
/// don't share inference ids across bindings, so per-binding letter
/// namespaces can't collide (Ide.collectTypedBindings makes the same call).
let private concreteValueTypes (ir: Blade.IR.IRProgram) : Map<string, string> =
    Map.ofList
        [ for m in ir.Modules do
            for b in m.Bindings do
                yield (b.Name, Blade.Ide.abstractRenderer [] b.Type) ]

/// The full tier's extra pass: the same lower + validateIR chain
/// `Interp.Repl.lowerSession` drives, but keeping failures STRUCTURED (code +
/// message) instead of rendering them rustc-style -- the IDE payload wants
/// diagnostics it can squiggle, not a formatted console block. Lowering can
/// THROW rather than return Error (a provider load that fails at compile
/// time), so the call is guarded.
let fullTierUpgrade : Blade.Ide.FullTierUpgrade =
    fun prog typed builder ->
        match (try Ok (Blade.Lowering.lowerTypedProgram typed (Some prog) builder)
               with ex -> Error [ ("BL6002", ex.Message) ]) with
        | Error failures -> Error failures
        | Ok ir ->
            match Blade.IRValidate.validateIR ir with
            | Error errs -> Error (errs |> List.map (fun e -> ("BL6001", e)))
            | Ok validated -> Ok (concreteValueTypes validated)

// Request decoding (System.Text.Json in, hand-rolled JSON out -- the payload
// itself is built by Ide.renderJson, so there is only ever one emitter).

let private tryProp (root: JsonElement) (name: string) : JsonElement option =
    match root.TryGetProperty name with
    | true, v -> Some v
    | _ -> None

let private tryStr (root: JsonElement) (name: string) : string option =
    tryProp root name
    |> Option.bind (fun v ->
        if v.ValueKind = JsonValueKind.String then Some (v.GetString()) else None)

let private tryInt (root: JsonElement) (name: string) : int option =
    tryProp root name
    |> Option.bind (fun v ->
        if v.ValueKind <> JsonValueKind.Number then None
        else match v.TryGetInt32() with
             | true, n -> Some n
             | _ -> None)

/// A JSON array of strings. All-or-nothing on purpose: a non-string element
/// makes the whole property absent, because silently dropping one notebook cell
/// would shift every window after it and misplace the client's squiggles.
let private tryStrList (root: JsonElement) (name: string) : string list option =
    tryProp root name
    |> Option.bind (fun v ->
        if v.ValueKind <> JsonValueKind.Array then None
        else
            let items = [ for e in v.EnumerateArray() -> e ]
            if items |> List.forall (fun e -> e.ValueKind = JsonValueKind.String)
            then Some (items |> List.map _.GetString())
            else None)

/// A JSON array of numbers, all-or-nothing for the same reason `tryStrList`
/// is: a camera with one unreadable slot is not a camera, and rendering from
/// a partial one would put the lens somewhere the caller never asked for.
let private tryNumList (root: JsonElement) (name: string) : float list option =
    tryProp root name
    |> Option.bind (fun v ->
        if v.ValueKind <> JsonValueKind.Array then None
        else
            let items = [ for e in v.EnumerateArray() -> e ]
            if items |> List.forall (fun e -> e.ValueKind = JsonValueKind.Number)
            then Some (items |> List.map _.GetDouble())
            else None)

/// The RAW JSON TEXT of an object-valued property; None when it is absent or
/// is not an object. `GetRawText` hands back the input's own bytes for that
/// element, which is the whole point for a render `spec`: it travels through
/// serve to the worker untouched, exactly as `evalResponse` below splices
/// display frames rather than re-serializing them. A parse-and-reprint round
/// trip is licensed to move the last digit of every coordinate in the plot.
let private tryRawObject (root: JsonElement) (name: string) : string option =
    tryProp root name
    |> Option.bind (fun v ->
        if v.ValueKind = JsonValueKind.Object then Some (v.GetRawText()) else None)

// The notebook lane's response encoder. Hand-rolled like the error response
// above: one line, no pretty printing, `Ide.jsonEscape` on every string that
// came from user source or a diagnostic.

let private laneName (l: Blade.ReplSession.Lane) =
    match l with
    | Blade.ReplSession.LaneInterp -> "interp"
    | Blade.ReplSession.LaneGpp -> "gpp"

/// Public (not private like its siblings) so tests/Test_Display.fs can pin the
/// WIRE BYTES of the `display` array rather than re-deriving them: the frames
/// are spliced in as raw JSON, which is exactly the thing a paraphrase would
/// get wrong.
let evalResponse (id: int) (r: Blade.ReplSession.EvalResult) : string =
    let sb = StringBuilder()
    let esc = Blade.Ide.jsonEscape
    sb.AppendFormat(
        "{{\"id\":{0},\"kept\":{1},\"exitCode\":{2},\"lane\":\"{3}\",\"elapsedMs\":{4}",
        id, (if r.Kept then "true" else "false"), r.ExitCode, laneName r.Lane, r.ElapsedMs) |> ignore
    sb.AppendFormat(",\"stdout\":\"{0}\",\"stderr\":\"{1}\",\"bindings\":[",
                    esc r.Stdout, esc r.Stderr) |> ignore
    r.Bindings
    |> List.iteri (fun i b ->
        if i > 0 then sb.Append ',' |> ignore
        sb.AppendFormat("{{\"name\":\"{0}\",\"type\":\"{1}\",\"value\":\"{2}\"}}",
                        esc b.Name, esc b.Type, esc b.Value) |> ignore)
    sb.Append "],\"diagnostics\":[" |> ignore
    r.Diagnostics
    |> List.iteri (fun i d ->
        if i > 0 then sb.Append ',' |> ignore
        sb.AppendFormat(
            "{{\"severity\":\"{0}\",\"line\":{1},\"col\":{2},\"endLine\":{3},\"endCol\":{4},\"message\":\"{5}\"",
            d.Severity, d.Line, d.Col, d.EndLine, d.EndCol, esc d.Message) |> ignore
        if d.Code <> "" then
            sb.AppendFormat(",\"code\":\"{0}\"", esc d.Code) |> ignore
        sb.Append '}' |> ignore)
    sb.Append ']' |> ignore
    // Display frames (docs/display-frames.md section 2). Each entry is ALREADY a
    // complete JSON object -- it travelled as one on stdout and
    // Blade.Display.Frame built it -- so it is spliced in verbatim, NOT
    // escaped: escaping it here would turn the array into an array of strings
    // and the reader would reject every one. The field is emitted only when
    // non-empty, so a program that never plots produces a byte-identical
    // response to the one this compiler produced before display frames existed.
    if not r.Display.IsEmpty then
        sb.Append ",\"display\":[" |> ignore
        r.Display
        |> List.iteri (fun i f ->
            if i > 0 then sb.Append ',' |> ignore
            sb.Append f |> ignore)
        sb.Append ']' |> ignore
    sb.Append '}' |> ignore
    sb.ToString()

// The GR render lane's response encoder (see the header). Same hand-rolled
// shape as the eval encoder above, and public for the same reason.

/// Default render size when the client names none. The panel reports its own
/// pixel size in the request; these are for callers that don't care.
[<Literal>]
let DefaultWidth = 800
[<Literal>]
let DefaultHeight = 600

/// Sizes outside this are refused-by-clamping rather than passed to GR: the
/// cairo device is hardwired to 600 dpi and a nonsense extent is a slow way to
/// find that out.
[<Literal>]
let MinDim = 64
[<Literal>]
let MaxDim = 4096

let clampDim (n: int) : int = max MinDim (min MaxDim n)

/// An optional pixel dimension: absent takes the default, present is clamped.
/// PRESENT-BUT-NOT-AN-INTEGER is an error rather than a silent fallback --
/// a client sending `"width":"800"` has a bug, and rendering at 800 anyway
/// would hide it.
let private tryDim (root: JsonElement) (name: string) (dflt: int) : Result<int, string> =
    match tryProp root name with
    | None -> Ok dflt
    | Some v ->
        if v.ValueKind <> JsonValueKind.Number then Error $"\"{name}\" must be an integer"
        else
            match v.TryGetInt32() with
            | true, n -> Ok (clampDim n)
            | _ -> Error $"\"{name}\" must be an integer"

/// The three formats the worker can print, and the mime each frame carries.
/// All three are binary as far as the frame format is concerned -- `svg+xml`
/// is neither `text/*` nor `+json` -- so all three travel base64, which is
/// what `Frame.headFor` derives for us rather than us restating it.
let mimeForFormat (format: string) : string =
    match format with
    | "svg" -> "image/svg+xml"
    | "pdf" -> "application/pdf"
    | _ -> "image/png"

/// One `renderPlot` response line. Public (not private like its siblings) so
/// tests/Test_GrRender.fs can pin the WIRE BYTES rather than re-derive them --
/// the same reason `evalResponse` is public, and the same hazard: `data` is
/// spliced in as one already-base64 string and the frame's leading half comes
/// from `Blade.Display.Frame.headFor`, so a paraphrase here would be free to
/// disagree with the format module about `v` or `encoding`.
///
/// `plotId` is echoed from the request and is what makes the panel MERGE this
/// render into an existing plot instead of appending a new one; when the client
/// sends none, `meta` carries only the backend.
let renderPlotResponse (id: int) (format: string) (plotId: string option) (data: string) : string =
    let head = Blade.Display.Frame.headFor (mimeForFormat format)
    let idEntry =
        match plotId with
        | Some p -> $"\"id\":\"{Blade.Display.Frame.escape p}\","
        | None -> ""
    $"{{\"id\":{id},\"frame\":{head}\"{data}\",\"meta\":{{{idEntry}\"backend\":\"gr\"}}}}}}"

// The loop

/// One request at a time, deliberately: `Ast.synthSpan` is a plain mutable
/// global, and serializing is what keeps a daemon honest about it. (The
/// parser's own per-parse state no longer needs this -- it is `[<ThreadStatic>]`
/// as of the span-table race fix, Parser.fs -- but one shared global is enough
/// to keep the rule.) Factored over
/// TextReader/TextWriter so the test suite can drive a whole conversation
/// in-process without spawning anything.
// ---------------------------------------------------------------------------
// THE RENDER FAST PATH
// ---------------------------------------------------------------------------
//
// A zoom gesture changes three numbers and nothing else, but seeing them costs
// a whole session evaluation: the camera cell is PART OF THE PROGRAM, so every
// gesture is a fresh compile -- or, in the interpreter, a fresh escape-time
// computation of every plot below the camera.
//
// `render` keeps the camera IN THE CELL. The client rewrites the cell text
// exactly as it does today, so the notebook stays truthful about where the lens
// points; what this lane COMPILES is a program the camera has been ERASED from,
// its named bindings reading their values out of a CSV at run time instead of
// carrying literals. That source is the same bytes for every camera, so the
// executable is built ONCE and re-run per gesture with only three numbers
// travelling. Measured on examples/mandelbrot.bladenb, one gesture:
//
//   interpreter, re-running the session      5480 ms
//   this lane, first call (builds)          ~5400 ms
//   this lane, every call after              423 ms
//
// The erasure is value-exact -- the notebook EXPECT pins reproduce bit for bit
// through the CSV -- and it is also the only thing that puts the executable
// cache (Build.fs) within reach: that cache is keyed on the emitted .cpp text,
// which a baked-in camera would change on every single gesture.

/// (srcPath) -> the built executable. Injected by Cli.fs, which owns
/// compileToExe: IdeServe.fs compiles before Build.fs, the same ordering that
/// makes ReplSession take its g++ fallback lane by injection.
type RenderCompile = string -> Result<string, string>

/// (cwd, exePath) -> exit code, stdout, stderr.
type RenderRun = string -> string -> Result<int * string * string, string>

let mutable private renderCompile : RenderCompile option = None
let mutable private renderRun : RenderRun option = None

/// Hand the render lane its toolchain driver. Called once, by Cli.fs.
let installRenderLane (compile: RenderCompile) (run: RenderRun) =
    renderCompile <- Some compile
    renderRun <- Some run

/// One row, one column per camera slot. "R" round-trips a Float64 exactly, so
/// the value the client chose is the value the picture is computed from.
let cameraCsvText (values: float list) : string =
    (values
     |> List.map (fun v -> v.ToString("R", Globalization.CultureInfo.InvariantCulture))
     |> String.concat ",") + "\n"

/// Rewrite `source` so each binding in `names` reads its value out of the CSV at
/// `csvPath` rather than carrying a literal, and hand back the whole program.
///
/// Refuses rather than guesses. A camera name bound zero times, or more than
/// once, would otherwise render from a camera the caller never set -- and the
/// picture would look perfectly plausible, which is the worst way to be wrong.
let cameraErasedSource (source: string) (names: string list) (csvPath: string)
                       : Result<string, string> =
    if List.isEmpty names then Error "no camera bindings were named" else
    // Forward slashes: this path lands inside a Blade string literal, where a
    // Windows separator would read as an escape.
    let path = csvPath.Replace("\\", "/")
    let located =
        names |> List.mapi (fun slot n ->
            let re =
                Text.RegularExpressions.Regex(
                    "(?m)^[ \t]*let[ \t]+"
                    + Text.RegularExpressions.Regex.Escape n + "[ \t]*=.*$")
            let ms = re.Matches source
            if ms.Count = 1 then Ok (slot, n, ms.[0])
            else
                Error (sprintf
                        "camera binding %s is bound %d times at top level (expected exactly once)"
                        n ms.Count))
    match located |> List.tryPick (function Error e -> Some e | Ok _ -> None) with
    | Some e -> Error e
    | None ->
        let hits = located |> List.choose (function Ok x -> Some x | Error _ -> None)
        // Rewrite from the LAST binding backwards, so the earlier offsets this
        // loop has yet to use are not disturbed by its own edits.
        let mutable out = source
        for (slot, n, m) in hits |> List.sortByDescending (fun (_, _, m) -> m.Index) do
            out <- out.Substring(0, m.Index)
                   + sprintf "let %s = __blade_cam_slots(0, %d)" n slot
                   + out.Substring(m.Index + m.Length)
        // The reader goes immediately above the EARLIEST camera binding: that
        // precedes every use of the camera without moving anything else in a
        // session whose declaration order belongs to the user.
        let firstIdx = hits |> List.map (fun (_, _, m) -> m.Index) |> List.min
        let preamble =
            sprintf "let __blade_cam_store = __blade_cam_csv.load(\"%s\")\n" path
            + "let __blade_cam_slots = __blade_cam_store.vars.data |> __blade_cam_csv.read\n"
        out <- out.Substring(0, firstIdx) + preamble + out.Substring firstIdx
        Ok ("import csv as __blade_cam_csv\n\n" + out)

/// Which frames of a render to actually send, as indices into this run's
/// `digests`, given the digests of what the session's LAST render showed.
///
/// A camera change can only alter the plots that read the camera, but the
/// program emits every plot it has -- so sending all of them replays the whole
/// notebook into the panel. The failure mode to guard is the other direction:
/// holding back a frame that DID move leaves a stale picture on screen, so
/// anything past the end of `previous` counts as changed.
let changedFrameIndices (previous: string[]) (digests: string[]) : int[] =
    [| for i in 0 .. digests.Length - 1 do
         if i >= previous.Length || previous.[i] <> digests.[i] then yield i |]

/// SHA256 of the erased source, hex -- the render cache key. Same bytes means
/// the executable already on disk still computes this program.
let private sourceDigest (s: string) : string =
    use sha = Security.Cryptography.SHA256.Create()
    sha.ComputeHash(Encoding.UTF8.GetBytes s)
    |> Array.map (fun b -> b.ToString "x2")
    |> String.concat ""

let serveLoop (version: string) (input: TextReader) (output: TextWriter) : int =
    // Exactly one \n-terminated line per response, flushed before the next
    // read: the client frames on newlines and correlates by id. WriteLine is
    // avoided on purpose -- it would emit CRLF on Windows.
    let respond (line: string) =
        output.Write line
        output.Write '\n'
        output.Flush ()
    let errorResponse (id: int option) (msg: string) =
        let idJson = match id with Some i -> string i | None -> "null"
        respond $"{{\"id\":{idJson},\"error\":\"{Blade.Ide.jsonEscape msg}\"}}"
    // Provider relative data paths resolve against the process working
    // directory, which one-shot `ide check` inherited from the editor (= the
    // checked file's directory). A persistent process has to reproduce that
    // per request. `file` is whatever the client sent and an unsaved buffer's
    // path need not exist, so the move is guarded rather than trusted.
    let setCwdFor (file: string) =
        try
            let dir = Path.GetDirectoryName(Path.GetFullPath file)
            if not (String.IsNullOrEmpty dir) && Directory.Exists dir then
                Directory.SetCurrentDirectory dir
        with _ -> ()
    let runCheck (id: int) (tier: string) (file: string) (source: string) =
        setCwdFor file
        // typeCheck resets its own AsyncLocal side-channels; these three it
        // does not touch. IdeStores would otherwise carry a PREVIOUS file's
        // provider stores into this payload, the icechunk axis mint table its
        // axis identities, and `provenance` is an append-only fold log that
        // would grow without bound in a process that never exits. The
        // mtime-keyed caches beside them are left alone -- they were built for
        // exactly this daemon shape.
        Blade.ProviderRegistry.IdeStores.reset ()
        Blade.IcechunkProvider.resetAxisMint ()
        Blade.ProviderStatics.provenance.Clear ()
        let env : Blade.Ide.Envelope = { Id = Some id; Tier = Some tier; Windows = None }
        let upgrade = if tier = "full" then Some fullTierUpgrade else None
        // The exit code is dropped: on this wire the diagnostics array carries
        // the same verdict, and the process outlives the check.
        let (json, _) = Blade.Ide.ideCheckSourceWith env upgrade file source
        respond json
    /// `check`'s notebook twin: the source is ASSEMBLED here, from the ordered
    /// code cells, instead of arriving pre-flattened -- the rebind and wrapper
    /// rules belong to Blade.ReplSession and the client keeps no copy of them.
    /// `windows` hands back where each cell landed. Nothing here reads or writes
    /// `sessions`: this command holds no state and commits nothing.
    let runCheckCells (id: int) (tier: string) (file: string) (cells: string list) =
        setCwdFor file
        // The same per-request hygiene `runCheck` applies, for the same reasons.
        Blade.ProviderRegistry.IdeStores.reset ()
        Blade.IcechunkProvider.resetAxisMint ()
        Blade.ProviderStatics.provenance.Clear ()
        let (source, windows) = Blade.ReplSession.assembleCells cells
        let env : Blade.Ide.Envelope = { Id = Some id; Tier = Some tier; Windows = Some windows }
        let upgrade = if tier = "full" then Some fullTierUpgrade else None
        let (json, _) = Blade.Ide.ideCheckSourceWith env upgrade file source
        respond json
    // One engine per `session` key, alive for the life of the process. The
    // sessions hold nothing but source snippets and a temp directory, so an
    // interleaved `check` cannot disturb them: each eval re-lowers the whole
    // session from its own list, and typeCheck resets its AsyncLocal channels
    // on the way in.
    let sessions = System.Collections.Generic.Dictionary<string, Blade.ReplSession.ReplSession>()
    let runEval (id: int) (key: string) (source: string) (cwd: string option) =
        // The notebook's folder, so a provider's relative data path resolves
        // where the user's data actually is. Same guarded move as `check`,
        // which resolves against the request file's directory instead.
        match cwd with
        | Some dir when Directory.Exists dir ->
            (try Directory.SetCurrentDirectory dir with _ -> ())
        | _ -> ()
        // The same per-request hygiene the check handler applies: a PREVIOUS
        // request's provider stores and axis identities must not follow this
        // evaluation, and `provenance` would otherwise grow without bound.
        Blade.ProviderRegistry.IdeStores.reset ()
        Blade.IcechunkProvider.resetAxisMint ()
        Blade.ProviderStatics.provenance.Clear ()
        let session =
            match sessions.TryGetValue key with
            | true, s -> s
            | _ ->
                let s = Blade.ReplSession.ReplSession(Directory.GetCurrentDirectory())
                sessions.[key] <- s
                s
        // Only the g++ lane reads this, and only to run the built executable.
        session.RunCwd <- Directory.GetCurrentDirectory()
        // Display-frame `meta.id`s are `<tag><ordinal>`, and the ordinal
        // restarts every run. That is what lets a session's re-run update the
        // editor's existing plots in place -- but two notebooks open at once
        // would then both claim `blade-1` and merge each other's plots. A tag
        // derived from the session key keeps them apart while staying STABLE
        // across that session's own re-runs. Set per request because one
        // process serves every open notebook; requests are handled strictly one
        // at a time (see the loop's doc comment), so this cannot interleave.
        Blade.Display.Frame.SessionTag <- Blade.Display.Frame.tagForSession key
        // STREAMED FRAMES (docs/display-frames.md section 3). A submission that
        // runs for minutes -- a training loop -- wants its plot refreshed while
        // it runs, not once at the end. The interpreter hands every
        // stream-mime frame to this sink AS IT IS PRODUCED (Frame.sink), and
        // each one goes out immediately as the out-of-band event line the
        // client already parses:
        //
        //   {"event":"display","id":<this request's id>,"frame":{...}}
        //
        // The composed line still carries its sentinel prefix (it is the same
        // bytes the buffered path would have put on stdout); the remainder IS
        // the frame object, spliced in raw for the same reason `display[]`
        // splices raw -- escaping it would hand the reader a string.
        //
        // A sunk frame is NOT buffered, so it never reaches stdout and never
        // appears in this response's `display` array: section 3 forbids
        // delivering one frame twice. The exception is a fall-back to the g++
        // lane, which re-runs the whole submission in a process with no sink
        // and therefore re-emits every frame into `display[]`; the panel merges
        // on `meta.id`, which for a stream frame is the stable channel name.
        //
        // Installed per request and cleared in a `finally`: serveLoop handles
        // one request at a time (see its doc comment), and EvalOnce is
        // synchronous, so a single mutable sink cannot interleave. Every other
        // lane -- `blade run`, `blade repl`, the corpus, the interp/g++
        // differential gate -- installs no sink and is untouched.
        let sentinel = Blade.Display.Frame.Sentinel
        Blade.Display.Frame.setSink (fun line ->
            let frame = if line.StartsWith sentinel then line.Substring sentinel.Length else line
            respond $"{{\"event\":\"display\",\"id\":{id},\"frame\":{frame}}}")
        let result =
            try session.EvalOnce source
            finally Blade.Display.Frame.clearSink ()
        respond (evalResponse id result)

    // session key -> (digest of the erased source, executable, camera CSV).
    // The point of the lane in one line: a gesture that changes only the
    // camera leaves the digest alone, so nothing recompiles and nothing is
    // even re-typechecked -- the front end is most of what is left once the
    // executable cache has turned g++ into a file copy.
    let renderCache = Collections.Generic.Dictionary<string, string * string * string>()
    // What the last render of this session PUT ON SCREEN: one digest per frame,
    // in program order. The same executable emits the same sequence every run,
    // so position identifies a plot across renders without parsing one.
    let renderFrames = Collections.Generic.Dictionary<string, string[]>()

    /// One camera change, rendered from an already-built executable.
    let runRender (id: int) (key: string) (names: string list) (values: float list)
                  (cwd: string option) =
        (match cwd with
         | Some dir when Directory.Exists dir ->
             (try Directory.SetCurrentDirectory dir with _ -> ())
         | _ -> ())
        let watch = Diagnostics.Stopwatch.StartNew()
        // Frame ids are `<tag><ordinal>` and CodeGen BAKES the tag into the
        // generated C++, so it has to be this session's before anything
        // compiles: the panel merges what this lane emits onto the entries the
        // eval lane already put there, and it merges them BY ID.
        Blade.Display.Frame.SessionTag <- Blade.Display.Frame.tagForSession key
        let outcome =
            match renderCompile, renderRun with
            | Some compile, Some run ->
                if names.Length <> values.Length then
                    Error (sprintf "\"render\" got %d bindings and %d values"
                               names.Length values.Length)
                else
                    match sessions.TryGetValue key with
                    | false, _ ->
                        Error (sprintf "session %s has evaluated nothing to render" key)
                    | true, session ->
                        let slug =
                            key |> String.map (fun c ->
                                if Array.contains c (Path.GetInvalidFileNameChars()) then '_' else c)
                        let dir = Path.Combine(Path.GetTempPath(), "blade-render", slug)
                        Directory.CreateDirectory dir |> ignore
                        let csvPath = Path.Combine(dir, "camera.csv")
                        let srcPath = Path.Combine(dir, "render.blade")
                        let source = String.concat "\n\n" session.Snippets + "\n"
                        match cameraErasedSource source names csvPath with
                        | Error e -> Error e
                        | Ok erased ->
                            // The CSV must exist BEFORE the compile: the
                            // provider reads its shape at compile time and
                            // bakes it, then re-validates at run time. Writing
                            // it first also means the very first render shows
                            // the camera the caller actually asked for.
                            File.WriteAllText(csvPath, cameraCsvText values)
                            let digest = sourceDigest erased
                            let hit =
                                match renderCache.TryGetValue key with
                                | true, (d, exe, _) when d = digest && File.Exists exe -> Some exe
                                | _ -> None
                            let built =
                                match hit with
                                | Some exe -> Ok (exe, true)
                                | None ->
                                    File.WriteAllText(srcPath, erased)
                                    // Same per-request hygiene the eval and
                                    // check handlers apply: this compiles a
                                    // program, and a previous request's stores
                                    // must not follow it in.
                                    Blade.ProviderRegistry.IdeStores.reset ()
                                    Blade.IcechunkProvider.resetAxisMint ()
                                    Blade.ProviderStatics.provenance.Clear ()
                                    match compile srcPath with
                                    | Error e -> Error e
                                    | Ok exe ->
                                        renderCache.[key] <- (digest, exe, csvPath)
                                        // A different program may emit a
                                        // different sequence, so position no
                                        // longer identifies anything: start the
                                        // comparison over.
                                        renderFrames.Remove key |> ignore
                                        Ok (exe, false)
                            match built with
                            | Error e -> Error e
                            | Ok (exe, wasCached) ->
                                match run (Directory.GetCurrentDirectory()) exe with
                                | Error e -> Error e
                                | Ok (code, out, err) -> Ok (out, err, code, wasCached)
            | _ -> Error "the render lane has no toolchain driver in this process"
        match outcome with
        | Error e -> errorResponse (Some id) e
        | Ok (out, err, code, wasCached) ->
            watch.Stop()
            // Frames go out as the live event lines the panel already consumes
            // for a stable-id plot, and NOT also in a `display` array: one
            // frame, one delivery (docs/display-frames.md section 3).
            let sentinel = Blade.Display.Frame.Sentinel
            let produced =
                out.Replace("\r\n", "\n").Split('\n')
                |> Array.filter (fun l -> l.StartsWith sentinel)
                |> Array.map (fun l -> l.Substring sentinel.Length)
            // A gesture moves the camera, and only the plots that READ the
            // camera can have changed by it -- but the program emits every plot
            // it has. Re-sending all of them replays the whole notebook into the
            // panel: a fixed tour under a stable id animates its entire descent
            // again on every zoom, and lands back where it started. So send the
            // frames whose bytes actually moved.
            //
            // Suppression is safe because the panel keeps history: an unchanged
            // frame would merge onto an entry already holding exactly those
            // bytes. The first render after a build sends everything -- there is
            // nothing to compare against, and that is the run whose output the
            // panel has not seen from this executable.
            let previous =
                match renderFrames.TryGetValue key with
                | true, ds -> ds
                | _ -> Array.empty
            let digests = produced |> Array.map sourceDigest
            let send = changedFrameIndices previous digests
            for i in send do
                respond (sprintf "{\"event\":\"display\",\"id\":%d,\"frame\":%s}" id produced.[i])
            let frames = send.Length
            renderFrames.[key] <- digests
            respond (sprintf
                        "{\"id\":%d,\"ok\":%b,\"cached\":%b,\"frames\":%d,\"unchanged\":%d,\"elapsedMs\":%d,\"exitCode\":%d,\"stderr\":\"%s\"}"
                        id (code = 0) wasCached frames (produced.Length - frames)
                        (int watch.ElapsedMilliseconds)
                        code (Blade.Ide.jsonEscape err))
    /// Handle one line; false means "stop the loop".
    let handle (line: string) : bool =
        use doc = JsonDocument.Parse line
        let root = doc.RootElement
        if root.ValueKind <> JsonValueKind.Object then
            errorResponse None "request must be a JSON object"
            true
        else
            let id = tryInt root "id"
            match tryStr root "cmd" with
            | Some "shutdown" -> false
            | Some "ping" ->
                (match id with
                 | Some i ->
                     respond ($"{{\"id\":{i},\"ok\":true,\"serve\":1,\"version\":\"{(Blade.Ide.jsonEscape version)}\"}}")
                 | None -> errorResponse None "\"ping\" requires an integer \"id\"")
                true
            | Some "check" ->
                (match id, tryStr root "file", tryStr root "source" with
                 | None, _, _ -> errorResponse None "\"check\" requires an integer \"id\""
                 | Some i, None, _ -> errorResponse (Some i) "\"check\" requires a \"file\" path"
                 | Some i, _, None -> errorResponse (Some i) "\"check\" requires a \"source\" string"
                 | Some i, Some file, Some source ->
                     match defaultArg (tryStr root "tier") "fast" with
                     | ("fast" | "full") as tier -> runCheck i tier file source
                     | other ->
                         errorResponse (Some i)
                             $"unknown tier '{other}' (expected \"fast\" or \"full\")")
                true
            | Some "checkCells" ->
                // No `session`: an empty `cells` array is a legitimate request
                // (a notebook with nothing but markdown in it) and answers with
                // an empty payload, but a MISSING one is a malformed request.
                (match id, tryStr root "file", tryStrList root "cells" with
                 | None, _, _ -> errorResponse None "\"checkCells\" requires an integer \"id\""
                 | Some i, None, _ -> errorResponse (Some i) "\"checkCells\" requires a \"file\" path"
                 | Some i, _, None -> errorResponse (Some i) "\"checkCells\" requires a \"cells\" array"
                 | Some i, Some file, Some cells ->
                     match defaultArg (tryStr root "tier") "fast" with
                     | ("fast" | "full") as tier -> runCheckCells i tier file cells
                     | other ->
                         errorResponse (Some i)
                             $"unknown tier '{other}' (expected \"fast\" or \"full\")")
                true
            | Some "eval" ->
                (match id, tryStr root "session", tryStr root "source" with
                 | None, _, _ -> errorResponse None "\"eval\" requires an integer \"id\""
                 | Some i, None, _ -> errorResponse (Some i) "\"eval\" requires a \"session\" key"
                 | Some i, _, None -> errorResponse (Some i) "\"eval\" requires a \"source\" string"
                 | Some i, Some key, Some source -> runEval i key source (tryStr root "cwd"))
                true
            | Some "render" ->
                (match id, tryStr root "session", tryStrList root "bindings",
                       tryNumList root "values" with
                 | None, _, _, _ -> errorResponse None "\"render\" requires an integer \"id\""
                 | Some i, None, _, _ ->
                     errorResponse (Some i) "\"render\" requires a \"session\" key"
                 | Some i, _, None, _ ->
                     errorResponse (Some i) "\"render\" requires a \"bindings\" array of strings"
                 | Some i, _, _, None ->
                     errorResponse (Some i) "\"render\" requires a \"values\" array of numbers"
                 | Some i, Some key, Some names, Some values ->
                     runRender i key names values (tryStr root "cwd"))
                true
            | Some "resetSession" ->
                // Idempotent by design: "restart kernel" fires before the
                // notebook has evaluated anything, and an unknown key simply
                // has no state to clear.
                (match id, tryStr root "session" with
                 | None, _ -> errorResponse None "\"resetSession\" requires an integer \"id\""
                 | Some i, None -> errorResponse (Some i) "\"resetSession\" requires a \"session\" key"
                 | Some i, Some key ->
                     (match sessions.TryGetValue key with
                      | true, s -> s.Reset()
                      | _ -> ())
                     respond $"{{\"id\":{i},\"ok\":true}}")
                true
            | Some "surface" ->
                // A pure read of tables compiled INTO this binary: no
                // setCwdFor, no store resets, no session touched, so it may
                // interleave with anything and needs none of `check`'s hygiene.
                (match id with
                 | Some i -> respond (Blade.Ide.renderSurfaceWith (Some i) version)
                 | None -> errorResponse None "\"surface\" requires an integer \"id\"")
                true
            | Some "renderPlot" ->
                // Reads no program and touches no session, so -- like `surface`
                // -- it needs none of `check`'s per-request hygiene and no
                // chdir. The spec is handed to the GR worker VERBATIM
                // (Blade.Display.GrRender owns the worker's whole lifecycle,
                // including the env that keeps GR from crashing silently) and
                // the bytes that come back are wrapped in a display frame.
                //
                // A failure here is an ordinary error response: no helper, no
                // GR, a worker that refused the spec, a render that timed out.
                // The panel keeps showing its plotly render either way.
                (match id with
                 | None -> errorResponse None "\"renderPlot\" requires an integer \"id\""
                 | Some i ->
                     match tryRawObject root "spec" with
                     | None -> errorResponse (Some i) "\"renderPlot\" requires a \"spec\" object"
                     | Some spec ->
                         let fmt =
                             match tryProp root "format" with
                             | None -> Ok "png"
                             | Some v ->
                                 if v.ValueKind <> JsonValueKind.String then
                                     Error "\"format\" must be a string"
                                 else
                                     match v.GetString() with
                                     | ("png" | "svg" | "pdf") as f -> Ok f
                                     | other ->
                                         Error $"unknown format '{other}' (expected \"png\", \"svg\" or \"pdf\")"
                         match fmt, tryDim root "width" DefaultWidth, tryDim root "height" DefaultHeight with
                         | Error e, _, _ | _, Error e, _ | _, _, Error e -> errorResponse (Some i) e
                         | Ok format, Ok width, Ok height ->
                             match Blade.Display.GrRender.render spec width height format with
                             | Error reason -> errorResponse (Some i) reason
                             | Ok data -> respond (renderPlotResponse i format (tryStr root "plotId") data))
                true
            | Some other ->
                errorResponse id $"unknown cmd '{other}'"
                true
            | None ->
                errorResponse id "request has no \"cmd\" string"
                true
    // Restored on the way out because the suite drives this loop in-process,
    // and a leaked chdir would follow every later test.
    let entryDir = try Some (Directory.GetCurrentDirectory()) with _ -> None
    try
        try
            let mutable running = true
            while running do
                // EOF (a closed stdin) is a clean shutdown, same as the verb.
                match input.ReadLine() with
                | null -> running <- false
                | line ->
                    // Tolerate the \r of a CRLF client, and ignore blank lines.
                    let trimmed = line.Trim()
                    if trimmed <> "" then
                        // NO input may kill the loop: the editor keeps typing
                        // through malformed frames and internal failures alike.
                        try running <- handle trimmed
                        with ex ->
                            // Bad framing is the client's business and gets one
                            // line; anything else is ours and earns the stack.
                            match ex with
                            | :? JsonException -> eprintfn "[ide serve] bad request: %s" ex.Message
                            | _ -> eprintfn "[ide serve] %s" (ex.ToString())
                            errorResponse (None: int option) ex.Message
        with :? IOException ->
            // The client went away mid-write; nothing left to say.
            ()
        0
    finally
        // Each session owns a temp directory; the loop outliving them all is
        // the only chance to remove it.
        for kv in sessions do kv.Value.Cleanup()
        sessions.Clear()
        // The GR worker is a CHILD PROCESS of this one and would otherwise
        // outlive it: nothing else ever kills it, and it is holding a GR
        // installation open. No-op when no render was ever asked for.
        (try Blade.Display.GrRender.shutdown () with _ -> ())
        match entryDir with
        | Some d -> (try Directory.SetCurrentDirectory d with _ -> ())
        | None -> ()

/// `blade ide serve`: wire the loop to the real console streams.
let serve (version: string) : int =
    // NDJSON is UTF-8 by contract; a redirected stdout on Windows would
    // otherwise inherit the console codepage. No BOM -- it would corrupt the
    // first response line.
    let out = new StreamWriter(Console.OpenStandardOutput(), UTF8Encoding(false))
    out.AutoFlush <- false
    // Any stray printfn from a deep compiler phase then lands in the SAME
    // buffer, in order, instead of racing a second writer onto the handle.
    Console.SetOut out
    let inp = new StreamReader(Console.OpenStandardInput(), UTF8Encoding(false))
    try serveLoop version inp out
    finally out.Flush ()
