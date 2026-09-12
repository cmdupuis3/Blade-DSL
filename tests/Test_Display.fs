// Display frames (Blade-REPL/docs/display-frames.md): the corpus categories
// plus the in-process block that pins the parts a .blade file cannot reach.
//
// The corpus half (`displayTests` / `displayErrorTests`) covers the SURFACE --
// that `display.emit(mime, data)` compiles, returns `true`, and rejects the
// three malformed usages -- and, through `blade test interp`, that the
// interpreter and the compiled binary put byte-identical frames on stdout.
//
// This file's own block covers what the corpus cannot:
//
//   * the exact BYTES of a frame (a corpus EXPECT only sees binding values);
//   * the escape table, including the sentinel's own SOH, on payloads that do
//     not have to survive Blade's string lexer first;
//   * the two CHANNELS -- the REPL's sentinel line on raw interpreter stdout
//     (spec section 4), and `ide serve`'s `display` array (spec section 2),
//     pinned on the real response encoder rather than a paraphrase of it.
module Blade.Tests.Display

open Blade.Tests.Corpus
open Blade.Tests.TestHarness

/// display-module tests (emit surface, encodings, both back ends).
let displayTests = category "display"

/// display-module reject probes (non-String payload, non-literal mime,
/// malformed mime). Their own category, like unit-errors and
/// mutability-errors: these never reach codegen, so they must not sit in the
/// category that feeds the interpreter differential.
let displayErrorTests = category "display-errors"

// The in-process block.

module F = Blade.Display.Frame

/// One check: name, condition, and the detail printed on failure.
let private checks = ResizeArray<string * (unit -> bool * string)>()
let private add name fn = checks.Add((name, fn))

let private eq (expected: string) (actual: string) =
    (expected = actual), (if expected = actual then "" else $"expected {expected}, got {actual}")

// ---- 1. The frame format ----------------------------------------------------

add "sentinel is SOH + 'blade-display' + SOH (15 bytes)" (fun () ->
    let codes = F.Sentinel |> Seq.map (fun c -> string (int c)) |> String.concat ","
    eq "1,98,108,97,100,101,45,100,105,115,112,108,97,121,1" codes)

add "encoding inferred: +json mime -> json" (fun () ->
    eq "json" (F.encodingFor "application/vnd.plotly.v1+json"))

add "encoding inferred: application/json -> json" (fun () ->
    eq "json" (F.encodingFor "application/json"))

add "encoding inferred: text/* -> utf8" (fun () ->
    eq "utf8" (F.encodingFor "text/html"))

add "encoding inferred: everything else -> base64" (fun () ->
    eq "base64" (F.encodingFor "image/png"))

add "mime grammar accepts type/subtype, rejects the rest" (fun () ->
    let ok = F.isMimeType "application/vnd.plotly.v1+json" && F.isMimeType "image/png"
    let bad = F.isMimeType "plotly" || F.isMimeType "a/b/c" || F.isMimeType "/png" || F.isMimeType ""
    (ok && not bad), sprintf "ok=%b bad=%b" ok bad)

add "escape covers quote, backslash and the sentinel's own SOH" (fun () ->
    // The SOH escape is the load-bearing one: it is what makes a payload
    // unable to forge a frame boundary, which is why the format needs no
    // escaping scheme of its own.
    eq "a\\\"b\\\\c\\u0001d\\n" (F.escape "a\"b\\cd\n"))

add "jsonString supplies its own quotes and escapes what is inside them" (fun () ->
    // The caller writes `"\"text\":" + jsonString t` -- delimiters included is
    // the whole point, because the bug this retires was a caller writing them
    // by hand around unescaped text.
    eq "\"he said \\\"hi\\\"\\\\\"" (F.jsonString "he said \"hi\"\\"))

add "jsonString of the empty string is a pair of quotes" (fun () ->
    eq "\"\"" (F.jsonString ""))

add "jsonNumber passes a finite rendering through untouched" (fun () ->
    eq "2.25" (F.jsonNumber "2.25" 2.25))

add "jsonNumber turns NaN and both infinities into null" (fun () ->
    // The guard reads the VALUE, not the rendering. The spelling of a
    // non-finite is implementation-defined (`nan`, `-nan`, `NaN`, `1.#QNAN`),
    // so a text test would pin the JSON rule to whichever formatter happened
    // to be underneath; both lanes branch on the finite predicate instead.
    let vals = [ nan; infinity; -infinity; -nan ]
    let outs = vals |> List.map (fun x -> F.jsonNumber "SHOULD-NOT-APPEAR" x)
    eq "null,null,null,null" (String.concat "," outs))

add "head carries v, mime and the inferred encoding" (fun () ->
    eq "{\"v\":1,\"mime\":\"image/png\",\"encoding\":\"base64\",\"data\":" (F.headFor "image/png"))

add "metaTailOf strips the braces and leads with a comma" (fun () ->
    eq ",\"title\":\"x\"" (defaultArg (F.metaTailOf "{\"title\":\"x\"}") "<none>"))

add "metaTailOf turns an empty object into an empty tail" (fun () ->
    eq "" (defaultArg (F.metaTailOf "{}") "<none>"))

add "metaTailOf rejects a non-object" (fun () ->
    let r = F.metaTailOf "\"title\""
    r.IsNone, sprintf "got %A" r)

add "a json-encoded frame inlines its payload unquoted" (fun () ->
    eq (F.Sentinel + "{\"v\":1,\"mime\":\"application/vnd.plotly.v1+json\",\"encoding\":\"json\",\"data\":{\"z\":[1]},\"meta\":{\"id\":\"blade-3\",\"title\":\"t\"}}")
       (F.composeLine (F.headFor "application/vnd.plotly.v1+json") false "{\"z\":[1]}" ",\"title\":\"t\"" 3))

add "a base64 frame quotes and escapes its payload" (fun () ->
    eq (F.Sentinel + "{\"v\":1,\"mime\":\"image/png\",\"encoding\":\"base64\",\"data\":\"iVBOR\",\"meta\":{\"id\":\"blade-1\"}}")
       (F.composeLine (F.headFor "image/png") true "iVBOR" "" 1))

// `display.emit_id`: the SAME line with a runtime `meta.id` spliced where
// `<SessionTag><ordinal>` goes. The whole point of pinning the bytes rather
// than the difference is that "the same line" is a claim about every other
// byte too -- head, payload, meta tail, both closing braces.

add "an emit_id frame splices the RUNTIME id where the ordinal goes" (fun () ->
    eq (F.Sentinel + "{\"v\":1,\"mime\":\"application/vnd.blade.plotstream.v1+json\",\"encoding\":\"json\","
        + "\"data\":{\"channel\":\"train_loss\",\"epoch\":3,\"x\":[0,1],\"y\":[0.9,0.5]},"
        + "\"meta\":{\"id\":\"train_loss\",\"stream\":true,\"backend\":\"plotly\"}}")
       (F.composeLineId (F.headFor F.StreamMime) false
            "{\"channel\":\"train_loss\",\"epoch\":3,\"x\":[0,1],\"y\":[0.9,0.5]}"
            ",\"stream\":true,\"backend\":\"plotly\"" "train_loss"))

add "an emit_id id is JSON-escaped like any other string value" (fun () ->
    // A channel name is program data. Unescaped, a `"` in it would close the
    // id and everything after would be read as sibling keys of `meta` -- the
    // same class of bug `json_string` retired one level down.
    // The SOH is spelled `char 1` rather than written into this literal: a raw
    // control byte in a source file is invisible to every reader and to most
    // editing tools.
    eq (F.Sentinel + "{\"v\":1,\"mime\":\"image/png\",\"encoding\":\"base64\",\"data\":\"AA==\",\"meta\":{\"id\":\"a\\\"b\\\\c\\u0001d\"}}")
       (F.composeLineId (F.headFor "image/png") true "AA==" "" ("a\"b\\c" + string (char 1) + "d")))

add "the stream mime is a +json mime, so its payload rides inline" (fun () ->
    // The frozen contract picks a `+json` mime precisely so the renderer gets
    // an object, not a string it has to parse a second time.
    eq "json" (F.encodingFor F.StreamMime))

add "the IR node is marked IMPURE (a future CSE/hoist must not drop it)" (fun () ->
    let node =
        Blade.IR.IRDisplayEmit (F.headFor "image/png", true,
                                Blade.IR.IRLit (Blade.IR.IRLitString "AA=="), "", None)
    let a = Blade.IRPrint.exprAttrs node
    (not a.IsPure) && (Blade.IRPrint.exprAttrs (Blade.IR.IRLit (Blade.IR.IRLitInt 1L))).IsPure,
    sprintf "IsPure = %b" a.IsPure)

// ---- 2. The REPL channel (spec section 4) -----------------------------------

/// Lower + interpret one source, returning the interpreter's RAW stdout --
/// i.e. what `blade repl` prints and what the extension's scanner reads.
/// The synthetic cwd-anchored entry path is what lets `import plot` /
/// `import units.SI` resolve against <repo>/stdlib exactly as a real
/// session does (lowerSession only resolves file imports when given a path).
let private interpStdout (source: string) : Result<string, string> =
    let entry =
        System.IO.Path.Combine(System.IO.Directory.GetCurrentDirectory(), "__display_tests__.blade")
    match Blade.Interp.Repl.lowerSession (Some entry) false source with
    | Error msg -> Error msg
    | Ok lowered ->
        match Blade.Interp.Repl.evalSession lowered "session" with
        | Blade.Interp.Repl.InterpDone r -> Ok r.Stdout
        | Blade.Interp.Repl.InterpFellShort f -> Error ("interpreter fell short: " + f)

let private emitSource =
    "import display as d\n\
     let ok = d.emit(\"application/vnd.plotly.v1+json\", \"{\\\"z\\\":[1,2]}\", \"{\\\"title\\\":\\\"t\\\"}\")\n"

add "REPL channel: the frame is a whole line at column 0 of stdout" (fun () ->
    match interpStdout emitSource with
    | Error e -> false, e
    | Ok out ->
        let lines = out.Replace("\r\n", "\n").Split('\n')
        match lines |> Array.tryFindIndex (fun l -> l.StartsWith F.Sentinel) with
        | None -> false, sprintf "no sentinel line in %A" lines
        | Some i ->
            // Column 0 by construction, and NOTHING but the frame on the line.
            let l = lines.[i]
            let json = l.Substring F.Sentinel.Length
            (json.StartsWith "{" && json.EndsWith "}" && not (json.Contains "\n")),
            $"line {i} = {json}")

add "REPL channel: the frame precedes the binding prints" (fun () ->
    // The compiled binary emits inside main()'s body, ahead of the timing line
    // and the print block; the interpreter has to agree or `blade test interp`
    // fails. Pinning the ORDER here catches the divergence without needing g++.
    match interpStdout emitSource with
    | Error e -> false, e
    | Ok out ->
        let lines = out.Replace("\r\n", "\n").Split('\n')
        let frameAt = lines |> Array.tryFindIndex (fun l -> l.StartsWith F.Sentinel)
        let bindAt = lines |> Array.tryFindIndex (fun l -> l.StartsWith "ok = ")
        match frameAt, bindAt with
        | Some f, Some b -> (f < b), $"frame at {f}, binding at {b}"
        | _ -> false, sprintf "missing line(s) in %A" lines)

add "REPL channel: ids count emissions and repeat across runs" (fun () ->
    // Two runs of the same program must produce the same ids -- that is what
    // makes a REPL session's re-run update the panel's plots in place.
    let src =
        "import display as d\n\
         let a = d.emit(\"image/png\", \"AA==\")\n\
         let b = d.emit(\"image/png\", \"BB==\")\n"
    let idsOf () =
        match interpStdout src with
        | Error e -> Error e
        | Ok out ->
            Ok (out.Replace("\r\n", "\n").Split('\n')
                |> Array.filter (fun l -> l.StartsWith F.Sentinel)
                |> Array.map (fun l ->
                    let i = l.IndexOf "\"id\":\""
                    l.Substring(i + 6, l.IndexOf("\"", i + 6) - i - 6))
                |> String.concat ",")
    match idsOf (), idsOf () with
    | Ok a, Ok b -> (a = "blade-1,blade-2" && a = b), $"run1={a} run2={b}"
    | Error e, _ | _, Error e -> false, e)

add "REPL channel: a program that never emits produces no sentinel" (fun () ->
    match interpStdout "let x = 1\n" with
    | Error e -> false, e
    | Ok out -> (not (out.Contains F.Sentinel)), out)

// ---- 3. The serve channel (spec section 2) ----------------------------------

/// One submission through the real REPL session engine -- the same call
/// `ide serve`'s `eval` command makes.
let private evalOnce (source: string) =
    let session = Blade.ReplSession.ReplSession(System.IO.Path.GetTempPath())
    try session.EvalOnce source
    finally session.Cleanup()

add "serve channel: eval carries the frame in Display" (fun () ->
    let r = evalOnce "import display as d\nlet ok = d.emit(\"image/png\", \"AA==\")"
    match r.Display with
    | [ f ] -> (f.StartsWith "{\"v\":1,\"mime\":\"image/png\"" && f.EndsWith "}"), f
    | other -> false, sprintf "expected 1 frame, got %A" other)

add "serve channel: frames are LIFTED OUT of stdout (never shown twice)" (fun () ->
    let r = evalOnce "import display as d\nlet ok = d.emit(\"image/png\", \"AA==\")"
    (not (r.Stdout.Contains F.Sentinel) && not (r.Stdout.Contains "blade-display")), r.Stdout)

add "serve channel: the submission is still kept and typed as usual" (fun () ->
    // The emitting call as the cell's final bare expression: the one display
    // slot a cell has. (A `let ok = d.emit(...)` spelling is kept too, but
    // declarations report no bindings.)
    let r = evalOnce "import display as d\nd.emit(\"image/png\", \"AA==\")"
    let b = r.Bindings |> List.tryFind (fun b -> b.Name = "")
    match b with
    | Some b -> (r.Kept && b.Type = "Bool" && b.Value = "true"), sprintf "kept=%b type=%s value=%s" r.Kept b.Type b.Value
    | None -> false, sprintf "no anonymous echo in %A" r.Bindings)

add "serve channel: a non-emitting submission has an empty Display" (fun () ->
    let r = evalOnce "let x = 1"
    r.Display.IsEmpty, sprintf "%A" r.Display)

add "serve wire: display[] splices raw frame objects, not strings" (fun () ->
    // The frames are already JSON objects; escaping them into the array would
    // hand the reader an array of strings and every frame would be rejected.
    let r = evalOnce "import display as d\nlet ok = d.emit(\"image/png\", \"AA==\")"
    let wire = Blade.IdeServe.evalResponse 7 r
    let doc = System.Text.Json.JsonDocument.Parse wire
    let mutable arr = Unchecked.defaultof<System.Text.Json.JsonElement>
    if not (doc.RootElement.TryGetProperty("display", &arr)) then false, wire
    else
        let items = [ for e in arr.EnumerateArray() -> e ]
        match items with
        | [ one ] ->
            (one.ValueKind = System.Text.Json.JsonValueKind.Object
             && one.GetProperty("mime").GetString() = "image/png"
             && one.GetProperty("encoding").GetString() = "base64"
             && one.GetProperty("data").GetString() = "AA=="
             && one.GetProperty("meta").GetProperty("id").GetString() = "blade-1"),
            wire
        | _ -> false, wire)

add "serve wire: display[] is omitted entirely when nothing was emitted" (fun () ->
    // Backward compatibility in the honest sense: a non-plotting submission's
    // response is byte-identical to what this compiler produced before display
    // frames existed.
    let wire = Blade.IdeServe.evalResponse 7 (evalOnce "let x = 1")
    (not (wire.Contains "display")), wire)

add "serve wire: a json-encoded payload lands as an OBJECT, not a string" (fun () ->
    let r = evalOnce "import display as d\nlet ok = d.emit(\"application/vnd.plotly.v1+json\", \"{\\\"z\\\":[1,2]}\")"
    let wire = Blade.IdeServe.evalResponse 1 r
    let doc = System.Text.Json.JsonDocument.Parse wire
    let f = (doc.RootElement.GetProperty "display").EnumerateArray() |> Seq.head
    let d = f.GetProperty "data"
    (d.ValueKind = System.Text.Json.JsonValueKind.Object
     && (d.GetProperty "z").GetArrayLength() = 2), wire)

// ---- 4. The plot module (stdlib/plot.blade) ---------------------------------
//
// Frame CONTENT checks the corpus cannot express: the payload parses as
// JSON, the trace/coloring/z land where plotly expects them, quantity-tagged
// slots steer the figure, and units.SI axis labels arrive via
// display.unit_label. Driven through the interpreter lane (interpStdout);
// the corpus + `blade test interp` pin compiled-lane byte parity.

let private plotFrame (source: string) : Result<System.Text.Json.JsonDocument, string> =
    match interpStdout source with
    | Error e -> Error e
    | Ok out ->
        let frames =
            out.Replace("\r\n", "\n").Split('\n')
            |> Array.filter (fun l -> l.StartsWith F.Sentinel)
        match frames with
        | [| l |] -> Ok (System.Text.Json.JsonDocument.Parse (l.Substring F.Sentinel.Length))
        | _ -> Error ($"expected exactly 1 frame, got {frames.Length}")

let private contourSource =
    "import plot\n\
     let ok = plot.contourf([0.0, 1.0, 2.0], [0.0, 1.0], [[0.0, 1.0, 2.25], [3.0, 4.0, 5.5]], 20: levels, \"waves\": title)\n"

add "plot.contourf: one frame, plotly mime, inline-json encoding" (fun () ->
    match plotFrame contourSource with
    | Error e -> false, e
    | Ok doc ->
        let r = doc.RootElement
        ((r.GetProperty "mime").GetString() = "application/vnd.plotly.v1+json"
         && (r.GetProperty "encoding").GetString() = "json"
         && (r.GetProperty "data").ValueKind = System.Text.Json.JsonValueKind.Object),
        r.ToString())

add "plot.contourf: the trace is a fill contour carrying the z grid" (fun () ->
    match plotFrame contourSource with
    | Error e -> false, e
    | Ok doc ->
        let trace = (doc.RootElement.GetProperty("data").GetProperty "data").EnumerateArray() |> Seq.head
        let z = trace.GetProperty "z"
        let row1 = z.EnumerateArray() |> Seq.item 1
        ((trace.GetProperty "type").GetString() = "contour"
         && (trace.GetProperty("contours").GetProperty "coloring").GetString() = "fill"
         && z.GetArrayLength() = 2
         && row1.EnumerateArray() |> Seq.map (_.GetDouble()) |> List.ofSeq = [3.0; 4.0; 5.5]),
        trace.ToString())

add "plot.contourf: tagged slots steer ncontours and the layout title" (fun () ->
    match plotFrame contourSource with
    | Error e -> false, e
    | Ok doc ->
        let data = doc.RootElement.GetProperty "data"
        let trace = (data.GetProperty "data").EnumerateArray() |> Seq.head
        ((trace.GetProperty "ncontours").GetInt32() = 20
         && (data.GetProperty("layout").GetProperty("title").GetProperty "text").GetString() = "waves"),
        data.ToString())

add "plot.line + units.SI: unit_label auto-fills the axis titles" (fun () ->
    let src =
        "import units.SI\n\
         import display as d\n\
         import plot\n\
         let ts: Array<Float<second> like Idx<3>> = [0.0, 1.0, 2.0]\n\
         let vs: Array<Float<meter/second^2> like Idx<3>> = [0.0, 9.81, 19.62]\n\
         let ok = plot.line(ts, vs, d.unit_label(ts): xlabel, d.unit_label(vs): ylabel)\n"
    match plotFrame src with
    | Error e -> false, e
    | Ok doc ->
        let layout = doc.RootElement.GetProperty("data").GetProperty "layout"
        ((layout.GetProperty("xaxis").GetProperty("title").GetProperty "text").GetString() = "second"
         && (layout.GetProperty("yaxis").GetProperty("title").GetProperty "text").GetString() = "meter / second^2"),
        layout.ToString())

add "plot: a title with quotes, a backslash and a tab still parses" (fun () ->
    // plotFrame PARSES the payload, so an unescaped title fails this check at
    // the parse, before the comparison -- which is exactly how the bug used to
    // present: one apostrophe-shaped character and the panel got nothing.
    let src =
        "import plot\n\
         let ok = plot.line([0.0, 1.0], [0.0, 1.0], \"he said \\\"hi\\\"\\tand\\\\left\": title)\n"
    match plotFrame src with
    | Error e -> false, e
    | Ok doc ->
        let layout = doc.RootElement.GetProperty("data").GetProperty "layout"
        let text = (layout.GetProperty("title").GetProperty "text").GetString()
        // Round trip: the reader hands back the ORIGINAL characters, not the
        // escapes -- the escaping is transport, not content.
        eq "he said \"hi\"\tand\\left" text)

add "plot: an axis label with a quote does not leak out of its string" (fun () ->
    let src =
        "import plot\n\
         let ok = plot.line([0.0, 1.0], [0.0, 1.0], \"x\\\" ,\\\"evil\\\":1\": xlabel)\n"
    match plotFrame src with
    | Error e -> false, e
    | Ok doc ->
        let layout = doc.RootElement.GetProperty("data").GetProperty "layout"
        // The injected `"evil":1` has to arrive as LABEL TEXT, never as a
        // sibling key of the layout object.
        let mutable leaked = Unchecked.defaultof<System.Text.Json.JsonElement>
        let escaped = layout.TryGetProperty("evil", &leaked)
        let text = (layout.GetProperty("xaxis").GetProperty("title").GetProperty "text").GetString()
        (not escaped && text = "x\" ,\"evil\":1"), sprintf "escaped=%b text=%s" escaped text)

add "plot: NaN and both infinities serialize as JSON null" (fun () ->
    let src =
        "import plot\n\
         let ok = plot.line([0.0, 1.0, 2.0, 3.0, 4.0], [2.5, 0.0 / 0.0, 1.0 / 0.0, -1.0 / 0.0, 0.5])\n"
    match plotFrame src with
    | Error e -> false, e
    | Ok doc ->
        let trace = (doc.RootElement.GetProperty("data").GetProperty "data").EnumerateArray() |> Seq.head
        let kinds =
            (trace.GetProperty "y").EnumerateArray()
            |> Seq.map (fun e ->
                match e.ValueKind with
                | System.Text.Json.JsonValueKind.Null -> "null"
                | System.Text.Json.JsonValueKind.Number -> string (e.GetDouble())
                | k -> sprintf "%A" k)
            |> String.concat ","
        // The finite samples are untouched: `null` is a gap marker, not a
        // blanket fallback.
        eq "2.5,null,null,null,0.5" kinds)

add "plot: json_num of a non-finite scalar slot is null too" (fun () ->
    // `ncontours` is the json_num path -- an Int slot here, so this check
    // drives the same serializer through a Float-typed figure field by way of
    // a NaN z grid, which is the only way a scalar slot can go non-finite in
    // v1. The z array covers json_array's rank-2 arm at the same time.
    let src =
        "import plot\n\
         let ok = plot.contourf([0.0, 1.0], [0.0, 1.0], [[0.0, 0.0 / 0.0], [1.0 / 0.0, 1.5]], 5: levels)\n"
    match plotFrame src with
    | Error e -> false, e
    | Ok doc ->
        let trace = (doc.RootElement.GetProperty("data").GetProperty "data").EnumerateArray() |> Seq.head
        let row (i: int) =
            (trace.GetProperty "z").EnumerateArray() |> Seq.item i
            |> fun r -> r.EnumerateArray() |> Seq.map (_.ValueKind.ToString()) |> String.concat ","
        ((trace.GetProperty "ncontours").GetInt32() = 5
         && row 0 = "Number,Null" && row 1 = "Null,Number"),
        $"row0={(row 0)} row1={(row 1)}")

// ---- 5. Grid decimation: the `maxdim` slot ----------------------------------
//
// A plotly figure costs ~20 bytes per sample, so a raw 2000x2000 z grid is
// ~80 MB -- over the display frame's 32 MB hard cap and far past what the
// panel can paint. The grid factories resample instead. What is pinned here
// is the OBSERVABLE contract: the emitted axis lengths, the sample positions
// (they are strided ORIGINALS, not interpolants), the byte-identity of the
// untouched path, and that the slot routes by nominal like every other one.
//
// The budget is a power of two <= maxdim rather than maxdim itself: a Blade
// array's extent lives in its TYPE, so a decimated axis needs a length the
// compiler knows, and plot.blade picks between per-budget helpers with a
// runtime `if`. The ladder rungs are pinned below because they ARE the
// surface -- `16: maxdim` giving 16 samples is what a caller sees.

/// A 40-wide by 24-tall grid, built in the language rather than written out:
/// 960 samples is past what a source literal should carry, and the factories
/// only ever read extents off it.
let private gridPrelude =
    "import plot\n\
     let gx = method_for(range<Idx<40>>) <@> lambda(i) -> 1.0 * i |> compute\n\
     let gy = method_for(range<Idx<24>>) <@> lambda(i) -> 1.0 * i |> compute\n\
     let gz = method_for(range<Idx<24>, Idx<40>>) <@> lambda(i, j) -> 1.0 * (i * 40 + j) |> compute\n"

/// The one trace of a one-trace figure.
let private traceOf (doc: System.Text.Json.JsonDocument) =
    (doc.RootElement.GetProperty("data").GetProperty "data").EnumerateArray() |> Seq.head

/// "<x len>,<y len>,<z rows>,<distinct z row lens>" -- one string so a
/// failure prints the whole shape rather than the first disagreement.
let private shapeOf (doc: System.Text.Json.JsonDocument) =
    let t = traceOf doc
    let z = t.GetProperty "z"
    let rowLens =
        z.EnumerateArray() |> Seq.map (fun r -> string (r.GetArrayLength()))
        |> Seq.distinct |> String.concat "/"
    $"""{((t.GetProperty "x").GetArrayLength())},{((t.GetProperty "y").GetArrayLength())},{(z.GetArrayLength())},{rowLens}"""

let private numbersOf (e: System.Text.Json.JsonElement) =
    e.EnumerateArray() |> Seq.map (fun v -> string (v.GetDouble())) |> String.concat ","

add "plot.contourf: an over-cap grid decimates x, y and z to one budget" (fun () ->
    // 40 and 24 both exceed a cap of 16, so every axis comes back at 16 --
    // one decision, one budget, and the trace stays rectangular.
    match plotFrame (gridPrelude + "let ok = plot.contourf(gx, gy, gz, 16: maxdim)\n") with
    | Error e -> false, e
    | Ok doc -> eq "16,16,16,16" (shapeOf doc))

add "plot.contourf: decimated samples are strided ORIGINALS, not interpolants" (fun () ->
    // Sample k reads source index (k * n) / B, so the picked values are
    // exactly the data at those positions -- and the LAST one falls short of
    // the data edge (37 of 39, 22 of 23), which is the documented v1 edge
    // behavior rather than an off-by-one.
    match plotFrame (gridPrelude + "let ok = plot.contourf(gx, gy, gz, 16: maxdim)\n") with
    | Error e -> false, e
    | Ok doc ->
        let t = traceOf doc
        let xs = numbersOf (t.GetProperty "x")
        let ys = numbersOf (t.GetProperty "y")
        // The z cell (i, j) has to agree with the axes it was sampled with:
        // row 1 of the decimated grid is source row 1 (= (1*24)/16), whose
        // first cell is 1*40 + 0 = 40.
        let z10 = ((t.GetProperty "z").EnumerateArray() |> Seq.item 1).EnumerateArray() |> Seq.head
        eq "0,2,5,7,10,12,15,17,20,22,25,27,30,32,35,37|0,1,3,4,6,7,9,10,12,13,15,16,18,19,21,22|40"
           ($"{xs}|{ys}|{(string (z10.GetDouble()))}"))

add "plot.contourf: a grid inside the cap is serialized BYTE-FOR-BYTE unchanged" (fun () ->
    // The whole no-op path in one assertion: with both axes under the
    // default cap of 512 the arrays reach the payload exactly as written,
    // through d.json_array and nothing else. The other guards on this path
    // are the three `contourSource` checks above (which read the same frame
    // for its trace/coloring/z and its tagged slots) and the compiled-lane
    // byte diff of tests/corpus/display/003_plot_contourf.blade.
    match interpStdout contourSource with
    | Error e -> false, e
    | Ok out ->
        let line =
            out.Replace("\r\n", "\n").Split('\n')
            |> Array.tryFind (fun l -> l.StartsWith F.Sentinel)
        match line with
        | None -> false, "no frame"
        | Some l ->
            (l.Contains "\"x\":[0,1,2],\"y\":[0,1],\"z\":[[0,1,2.25],[3,4,5.5]]"), l)

add "plot: the maxdim budget snaps DOWN to a supported power of two" (fun () ->
    // 20 is not a budget; 16 is the largest one at or below it. 32 is a
    // budget and is used as-is. Anything under the smallest rung floors at
    // 16 rather than degenerating.
    let shapeAt (cap: string) =
        match plotFrame (gridPrelude + $"let ok = plot.contourf(gx, gy, gz, {cap}: maxdim)\n") with
        | Error e -> "ERR:" + e
        | Ok doc -> shapeOf doc
    eq "16,16,16,16|32,32,32,32|16,16,16,16"
       ($"""{(shapeAt "20")}|{(shapeAt "32")}|{(shapeAt "2")}"""))

add "plot: a grid with BOTH axes at or under maxdim is left alone" (fun () ->
    // The trigger is `>`, not `>=`: a cap of 40 leaves the 40-wide axis
    // exactly where it is, and the 24-tall one with it.
    match plotFrame (gridPrelude + "let ok = plot.contourf(gx, gy, gz, 40: maxdim)\n") with
    | Error e -> false, e
    | Ok doc -> eq "40,24,24,40" (shapeOf doc))

add "plot: EITHER axis over the cap decimates BOTH" (fun () ->
    // 40 > 32 while 24 < 32, and the short axis is resampled too -- the
    // documented v1 rule, and what bounds a frame at budget^2 samples
    // whatever the input aspect ratio.
    match plotFrame (gridPrelude + "let ok = plot.contourf(gx, gy, gz, 32: maxdim)\n") with
    | Error e -> false, e
    | Ok doc -> eq "32,32,32,32" (shapeOf doc))

add "plot: the maxdim slot routes by NOMINAL, flat and chained" (fun () ->
    // The factory-slot contract through the real module, exactly as
    // tests/corpus/display/004_plot_factory_slots.blade exercises the older
    // slots: declaration order is levels, cmap, maxdim, so both of these
    // hand them over out of order, and the chained spelling splits them
    // across three groups. All three have to land in the same figure.
    let flat =
        gridPrelude + "let ok = plot.contourf(gx, gy, gz, 16: maxdim, 1: cmap, 7: levels)\n"
    let chained =
        gridPrelude + "let ok = plot.contourf(gx, gy, gz)(7: levels)(16: maxdim)(1: cmap)\n"
    let readBack (src: string) =
        match plotFrame src with
        | Error e -> "ERR:" + e
        | Ok doc ->
            let t = traceOf doc
            $"""{(shapeOf doc)}|{((t.GetProperty "colorscale").GetString())}|{((t.GetProperty "ncontours").GetInt32())}"""
    eq "16,16,16,16|Plasma|7|16,16,16,16|Plasma|7"
       ($"{(readBack flat)}|{(readBack chained)}"))

add "plot: contour and heatmap carry the same maxdim slot" (fun () ->
    // All three GRID factories decimate; line/scatter have no grid to
    // decimate and deliberately do not take the slot.
    let shapeFor (call: string) =
        match plotFrame (gridPrelude + call) with
        | Error e -> "ERR:" + e
        | Ok doc -> shapeOf doc
    eq "16,16,16,16|16,16,16,16"
       (sprintf "%s|%s"
            (shapeFor "let ok = plot.contour(gx, gy, gz, 16: maxdim)\n")
            (shapeFor "let ok = plot.heatmap(gx, gy, gz, 16: maxdim)\n")))

// ---- 6. The `backend` slot: a PREFERENCE, not a change of content -----------
//
// `meta.backend` names the backend that PRODUCED a render, and the panel keys
// its per-plot render cache on it -- a plotly payload stamped
// `"backend":"gr"` would be filed AS the GR render and permanently suppress
// the real one. A program's PREFERENCE is therefore a separate, ADDITIVE key,
// `preferredBackend`, and everything else about the frame stays where it was.
//
// The default emits no key at all, which is the load-bearing half: an
// untagged call's meta is byte-for-byte what this module emitted before the
// slot existed, so every frame pin above (and the corpus differential) keeps
// guarding exactly what it guarded.
//
// `display.emit`'s meta must be a string LITERAL -- the head and meta tail are
// computed once at elaboration time, so even `"{\"a\":1" + "}"` is refused
// with BL5700 "display.emit: the meta argument must be a string literal".
// plot.blade therefore picks between two COMPLETE literals with a runtime
// `if` instead of building one. Both arms are pinned here byte-for-byte.

/// The one frame line of a one-frame program, sentinel stripped.
let private frameLineOf (source: string) : string =
    match interpStdout source with
    | Error e -> "ERR:" + e
    | Ok out ->
        match out.Replace("\r\n", "\n").Split('\n') |> Array.tryFind (fun l -> l.StartsWith F.Sentinel) with
        | None -> "ERR:no frame"
        | Some l -> l.Substring F.Sentinel.Length

/// That frame's `"meta":{...}` object as RAW BYTES -- the frame's own closing
/// brace trimmed off, nothing else touched. Reading the text rather than a
/// parse is the point: key ORDER and the absence of a key are both contract.
let private metaBytesOf (source: string) : string =
    let l = frameLineOf source
    if l.StartsWith "ERR:" then l
    else
        let i = l.IndexOf "\"meta\":"
        if i < 0 then "ERR:no meta in " + l
        else
            let m = l.Substring i
            if m.EndsWith "}" then m.Substring(0, m.Length - 1) else m

/// `plot.line` with the given extra slot text (`""` for none).
let private lineSlots (slots: string) =
    $"import plot\nlet ok = plot.line([0.0, 1.0], [0.0, 1.0]{slots})\n"

add "plot: the default meta carries id + backend and NOTHING else" (fun () ->
    // Byte-identical to what plot.blade emitted before the slot existed. Any
    // key added here -- including a `preferredBackend` leaking onto the
    // default path -- fails this line.
    eq "\"meta\":{\"id\":\"blade-1\",\"backend\":\"plotly\"}" (metaBytesOf (lineSlots "")))

add "plot: 1: backend ADDS preferredBackend and leaves backend on plotly" (fun () ->
    // `backend` staying "plotly" is the whole contract: the payload IS plotly
    // JSON, so the key that says who produced it must keep saying plotly.
    eq "\"meta\":{\"id\":\"blade-1\",\"backend\":\"plotly\",\"preferredBackend\":\"gr\"}"
       (metaBytesOf (lineSlots ", 1: backend")))

add "plot: 0: backend is the default arm, byte-identical to omitting it" (fun () ->
    eq (metaBytesOf (lineSlots "")) (metaBytesOf (lineSlots ", 0: backend")))

add "plot: an out-of-table backend index falls back to the default" (fun () ->
    // A backend choice is presentation, not data -- the same rule `cmap` uses
    // for an unknown colormap. 9 is not a backend, and the frame it produces
    // is the ordinary plotly one rather than a refusal or a broken meta.
    eq (metaBytesOf (lineSlots "")) (metaBytesOf (lineSlots ", 9: backend")))

add "plot: the backend slot moves the META and nothing else" (fun () ->
    // The other half of "it is a hint": a viewer that ignores
    // `preferredBackend` has to be handed the very same figure. Everything
    // ahead of `"meta":` -- version, mime, encoding and the whole payload --
    // is compared here.
    let upToMeta (s: string) =
        let i = s.IndexOf "\"meta\":"
        if i < 0 then "ERR:no meta in " + s else s.Substring(0, i)
    eq (upToMeta (frameLineOf (lineSlots ", \"waves\": title")))
       (upToMeta (frameLineOf (lineSlots ", \"waves\": title, 1: backend"))))

add "plot: all five factories carry the backend slot, flat and chained" (fun () ->
    // Declaration order puts `backend` last, so every one of these hands it
    // over out of order or in its own chained group -- routing by NOMINAL,
    // exactly like tests/corpus/display/004_plot_factory_slots.blade drives
    // the older slots.
    let g = "[0.0, 1.0], [0.0, 1.0], [[1.0, 2.0], [3.0, 4.0]]"
    let calls =
        [ $"plot.contourf({g}, 1: backend, 5: levels)"
          $"plot.contour({g})(1: backend)(3: levels)"
          $"plot.heatmap({g}, 1: backend, 2: cmap)"
          "plot.line([0.0, 1.0], [0.0, 1.0], 1: backend, \"t\": title)"
          "plot.scatter([0.0, 1.0], [0.0, 1.0])(1: backend)" ]
    let got =
        calls |> List.mapi (fun i c ->
            let m = metaBytesOf ($"import plot\nlet ok = {c}\n")
            if m = "\"meta\":{\"id\":\"blade-1\",\"backend\":\"plotly\",\"preferredBackend\":\"gr\"}"
            then "gr" else $"[{i} {m}]")
    eq "gr,gr,gr,gr,gr" (String.concat "," got))

// ---- 7. plot.stream: the live-plot channel ----------------------------------
//
// `plot.stream(name, x, y[, slots])` is the other frame shape this module
// emits -- one instalment of a chart a long-running cell keeps extending,
// under its own mime and carrying a backend-neutral
// {channel, epoch, x, y, labels} object instead of a plotly figure. The
// corpus (display/012) pins the surface and the two-lane byte parity; what is
// pinned here is the PAYLOAD's shape and the identity rule the panel merges
// on, neither of which a `// EXPECT:` can see.

let private streamSource =
    "import plot\n\
     let ok = plot.stream(\"train_loss\", [0.0, 1.0, 2.0], [0.9, 0.5, 0.3], \"mse\": ylabel, 3: epoch, \"batch\": xlabel, \"loss\": title)\n"

add "plot.stream: the stream mime, inline-json encoding, and one frame" (fun () ->
    match plotFrame streamSource with
    | Error e -> false, e
    | Ok doc ->
        let r = doc.RootElement
        ((r.GetProperty "mime").GetString() = F.StreamMime
         && (r.GetProperty "encoding").GetString() = "json"
         && (r.GetProperty "data").ValueKind = System.Text.Json.JsonValueKind.Object),
        r.ToString())

add "plot.stream: the payload carries channel, epoch, x, y and the labels" (fun () ->
    // The slots are handed over OUT of declaration order above, so this also
    // pins that `epoch` routes by nominal beside the three label slots.
    match plotFrame streamSource with
    | Error e -> false, e
    | Ok doc ->
        let d = doc.RootElement.GetProperty "data"
        let nums (name: string) =
            (d.GetProperty name).EnumerateArray() |> Seq.map (fun v -> string (v.GetDouble())) |> String.concat ","
        eq "train_loss|3|0,1,2|0.9,0.5,0.3|loss|batch|mse"
           ($"""{((d.GetProperty "channel").GetString())}|{((d.GetProperty "epoch").GetInt32())}|{(nums "x")}|{(nums "y")}|{((d.GetProperty "title").GetString())}|{((d.GetProperty "xlabel").GetString())}|{((d.GetProperty "ylabel").GetString())}"""))

add "plot.stream: meta.id IS the channel name, and it carries the stream flag" (fun () ->
    // Read as BYTES, like the backend-slot pins: key order and the absence of
    // any other key are both contract. This is the frozen wire meta.
    eq "\"meta\":{\"id\":\"train_loss\",\"stream\":true,\"backend\":\"plotly\"}"
       (metaBytesOf streamSource))

add "plot.stream: a bare call marks the epoch -1 and OMITS every label" (fun () ->
    // "not given" and "given as blank" are the same request, and the shorter
    // payload is the one a training loop repeats thousands of times.
    let src = "import plot\nlet ok = plot.stream(\"c\", [0.0, 1.0], [2.0, 3.0])\n"
    match plotFrame src with
    | Error e -> false, e
    | Ok doc ->
        let d = doc.RootElement.GetProperty "data"
        let has (k: string) =
            let mutable v = Unchecked.defaultof<System.Text.Json.JsonElement>
            d.TryGetProperty(k, &v)
        ((d.GetProperty "epoch").GetInt32() = -1
         && not (has "title") && not (has "xlabel") && not (has "ylabel")),
        d.ToString())

add "plot.stream: frames of one channel share an id, and spend NO ordinal" (fun () ->
    // Two instalments of one chart merge because their ids are equal; the
    // ordinary figure emitted after them is still `blade-1`, because
    // `display.emit_id` deliberately leaves the run's ordinal counter alone.
    // A stream that consumed ordinals would renumber -- and so detach -- every
    // ordinary plot in a notebook the moment a training cell ran.
    let src =
        "import plot\n\
         let a = plot.stream(\"loss\", [0.0], [1.0])\n\
         let b = plot.stream(\"loss\", [1.0], [0.5])\n\
         let c = plot.line([0.0, 1.0], [0.0, 1.0])\n"
    match interpStdout src with
    | Error e -> false, e
    | Ok out ->
        let ids =
            out.Replace("\r\n", "\n").Split('\n')
            |> Array.filter (fun l -> l.StartsWith F.Sentinel)
            |> Array.map (fun l ->
                let i = l.IndexOf "\"id\":\""
                l.Substring(i + 6, l.IndexOf("\"", i + 6) - i - 6))
            |> String.concat ","
        eq "loss,loss,blade-1" ids)

add "plot.stream: a channel name with a quote cannot escape meta.id" (fun () ->
    // The id travels through Frame.escape; the SAME name also travels through
    // d.json_string into the payload's `channel`. Both have to survive, and
    // the frame has to still PARSE -- which plotFrame checks by construction.
    let src = "import plot\nlet ok = plot.stream(\"a\\\" ,\\\"evil\\\":1\", [0.0], [1.0])\n"
    match plotFrame src with
    | Error e -> false, e
    | Ok doc ->
        let meta = doc.RootElement.GetProperty "meta"
        let mutable leaked = Unchecked.defaultof<System.Text.Json.JsonElement>
        let escaped = meta.TryGetProperty("evil", &leaked)
        ((not escaped)
         && (meta.GetProperty "id").GetString() = "a\" ,\"evil\":1"
         && (doc.RootElement.GetProperty("data").GetProperty "channel").GetString() = "a\" ,\"evil\":1"),
        meta.ToString())

add "plot.stream: no sink installed means an ordinary buffered frame" (fun () ->
    // The load-bearing default. Every lane but `ide serve` -- `blade run`,
    // `blade repl`, the corpus, the interp/g++ differential gate -- leaves the
    // sink unset, and a stream frame is then just another sentinel line at
    // column 0 of stdout, ahead of the binding prints like any other frame.
    let src = "import plot\nlet ok = plot.stream(\"loss\", [0.0], [1.0])\n"
    match interpStdout src with
    | Error e -> false, e
    | Ok out ->
        let lines = out.Replace("\r\n", "\n").Split('\n')
        let frameAt = lines |> Array.tryFindIndex (fun l -> l.StartsWith F.Sentinel)
        let bindAt = lines |> Array.tryFindIndex (fun l -> l.StartsWith "ok = ")
        match frameAt, bindAt with
        | Some f, Some b -> (f < b && F.sink.IsNone), $"frame at {f}, binding at {b}, sink set = {F.sink.IsSome}"
        | _ -> false, sprintf "missing line(s) in %A" lines)

add "the sink takes stream frames only, and leaves the buffer alone" (fun () ->
    // The sink's whole contract in one check: install one, run a program that
    // emits BOTH kinds, and the stream frame arrives live while the plotly
    // frame still travels the buffered stdout path. Cleared in a `finally`
    // exactly as IdeServe does, so a failure here cannot leak into the checks
    // that follow.
    let seen = ResizeArray<string>()
    let src =
        "import plot\n\
         let s = plot.stream(\"loss\", [0.0], [1.0])\n\
         let p = plot.line([0.0, 1.0], [0.0, 1.0])\n"
    let out =
        F.setSink (fun l -> seen.Add l)
        try interpStdout src finally F.clearSink ()
    match out with
    | Error e -> false, e
    | Ok text ->
        let frames =
            text.Replace("\r\n", "\n").Split('\n')
            |> Array.filter (fun l -> l.StartsWith F.Sentinel)
        let sunkIsStream =
            seen.Count = 1 && seen.[0].StartsWith (F.Sentinel + F.headFor F.StreamMime)
        // The stream frame is NOT also on stdout: section 3 of the spec
        // forbids delivering one frame twice.
        let stdoutIsPlotlyOnly =
            frames.Length = 1 && frames.[0].Contains "application/vnd.plotly.v1+json"
        (sunkIsStream && stdoutIsPlotlyOnly),
        $"sunk={seen.Count} stdoutFrames={frames.Length}")

// ---- Runner -----------------------------------------------------------------

/// Run the in-process display block. No compiler toolchain: the REPL-channel
/// checks drive the interpreter directly and the serve-channel checks drive the
/// real session engine and response encoder, so this block is cheap and
/// unconditional.
let runDisplayTests () : BlockResult =
    printHeader "Display Frames"
    let mutable passed = 0
    let mutable failed = 0
    let mutable failedNames = []
    for (name, fn) in checks do
        let (ok, detail) =
            try fn () with ex -> false, $"exception: {ex.Message}"
        if ok then
            passed <- passed + 1
            resultLine Pass name ""
        else
            failed <- failed + 1
            failedNames <- failedNames @ [name]
            resultLine Fail name detail
    printFooter "Display" [$"{passed} passed"; $"{failed} failed"]
    { Block = "Display"; Passed = passed; Failed = failed; Skipped = 0; FailedNames = failedNames }
