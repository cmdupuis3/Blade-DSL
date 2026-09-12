/// Input manifests and run records (docs/plans/plan-fortran-killer-2.md
/// section 7): the inputs a program depends on, as a compile-time TABLE the
/// compiler can print (`blade plan`) and bake into the executable, which
/// pairs it at exit with what the run observed and how it ended
/// (src/cpp/blade_run_record.hpp, written to BLADE_RUN_RECORD).
///
/// Two kinds of input reach the manifest:
///   - RUNTIME reads (`view |> alias.read`, `.stream`, `.read_window`,
///     `load_compound`): every `IRModule.ProviderReads` entry, named by its
///     receiving binding. Identity policy `version`: the executable records
///     the store's size and modification time when it exits; the values it
///     computed depend on content it never hashed.
///   - FOLDED reads: inputs the COMPILER read whole and turned into constants
///     (ProviderStatics' fold). Identity policy `content`: the SHA-256 the
///     fold was taken over IS the identity the executable's values depend on,
///     and it is baked as such. They are logged per compilation by
///     ProviderStatics (`foldLog`), so a harness compiling many programs in
///     one process attributes each fold to its own program.
///
/// The manifest is program-derived only -- no environment reaches the
/// emitted text through it. Build policy (march / fp-contract / reassoc) and
/// the library routes actually linked travel as `-DBLADE_RR_*` defines on the
/// g++ command line (Build.fs), which the executable cache already keys on.
module Blade.RunRecord

open System.Text
open Blade.Types
open Blade.IR

/// The compiler's version string, as the CLI prints it and the run record
/// bakes it.
let bladeVersion = "0.20.0"

type InputEntry = {
    /// Logical name: the receiving binding for a runtime read, the store
    /// variable for a folded one.
    Name: string
    Provider: string
    Path: string
    Variable: string
    /// Element type spelled as Blade spells it (`Float64`, `Int64`, ...);
    /// "?" when the IR element is not a primitive scalar.
    Elem: string
    /// Ordered axes: (index tag or `Idx`, extent or None when not static).
    Axes: (string * int64 option) list
    /// `dense` | `packed` | `compound` | `window` | `stream` | `folded`
    Storage: string
    Units: string option
    /// `version` | `content`
    Identity: string
    ContentHash: string option
}

let private elemName (t: IRType) : string * string option =
    let rec go (t: IRType) (unit: string option) =
        match t with
        | IRTScalar ETFloat64 -> "Float64", unit
        | IRTScalar ETFloat32 -> "Float32", unit
        | IRTScalar ETInt64 -> "Int64", unit
        | IRTScalar ETInt32 -> "Int32", unit
        | IRTScalar ETComplex128 -> "Complex128", unit
        | IRTScalar ETComplex64 -> "Complex64", unit
        | IRTScalar ETBool -> "Bool", unit
        | IRTScalar ETString -> "String", unit
        | IRTScalar ETUnit -> "Unit", unit
        | IRTUnitAnnotated (inner, u) -> go inner (Some (ppUnitSig u))
        | IRTIdxTagged (inner, _) -> go inner unit
        | _ -> "?", unit
    go t None

let private axesOf (at: IRArrayType) : (string * int64 option) list =
    at.IndexTypes |> List.map (fun ix ->
        let tag = match ix.Tag with Some t -> t | None -> (if ix.Symmetry <> SymNone && ix.Rank >= 2 then "packed" else "Idx")
        let ext = match ix.Extent with IRLit (IRLitInt n) -> Some n | _ -> None
        tag, ext)

let private storageOf (spec: ProviderReadSpec) : string =
    let packed = spec.VarType.IndexTypes |> List.exists (fun ix -> ix.Symmetry <> SymNone && ix.Rank >= 2)
    if spec.Streamed then "stream"
    elif spec.MaskName.IsSome then "compound"
    elif spec.Window.IsSome then "window"
    elif packed then "packed"
    else "dense"

/// The manifest of a lowered program: runtime reads from every module's
/// `ProviderReads` (in binding order), then the folds this compilation
/// logged. `folds` is `(provider, path, variable, hash)` per folded input.
let manifestOf (modules: IRModule list) (folds: (string * string * string * string) list) : InputEntry list =
    let runtime =
        modules |> List.collect (fun m ->
            m.Bindings |> List.choose (fun b ->
                match Map.tryFind b.Id m.ProviderReads with
                | None -> None
                | Some spec ->
                    let elem, units = elemName spec.VarType.ElemType
                    Some { Name = b.Name; Provider = spec.Provider; Path = spec.FilePath; Variable = spec.VarName
                           Elem = elem; Axes = axesOf spec.VarType; Storage = storageOf spec; Units = units
                           Identity = "version"; ContentHash = None }))
    let folded =
        folds |> List.distinct |> List.map (fun (provider, path, var, hash) ->
            { Name = var; Provider = provider; Path = path; Variable = var; Elem = "?"; Axes = []
              Storage = "folded"; Units = None; Identity = "content"; ContentHash = Some hash })
    runtime @ folded

// ---------------------------------------------------------------------------
// Rendering: `blade plan` text and JSON
// ---------------------------------------------------------------------------

let private jsonString (s: string) : string =
    let sb = StringBuilder()
    sb.Append('"') |> ignore
    for c in s do
        match c with
        | '"' -> sb.Append("\\\"") |> ignore
        | '\\' -> sb.Append("\\\\") |> ignore
        | '\n' -> sb.Append("\\n") |> ignore
        | '\r' -> sb.Append("\\r") |> ignore
        | '\t' -> sb.Append("\\t") |> ignore
        | c when c < ' ' -> sb.AppendFormat("\\u{0:x4}", int c) |> ignore
        | c -> sb.Append(c) |> ignore
    sb.Append('"') |> ignore
    sb.ToString()

let private axesJson (axes: (string * int64 option) list) : string =
    axes
    |> List.map (fun (tag, ext) ->
        let e = match ext with Some n -> string n | None -> "null"
        $"{{\"tag\":{jsonString tag},\"extent\":{e}}}")
    |> String.concat ","
    |> fun body -> "[" + body + "]"

let private axesText (axes: (string * int64 option) list) : string =
    axes
    |> List.map (fun (tag, ext) -> match ext with Some n -> $"{tag}={n}" | None -> $"{tag}=?")
    |> String.concat " x "

let renderEntry (e: InputEntry) : string =
    let shape =
        match e.Storage with
        | "folded" -> "folded at compile time"
        | s -> $"{s} {e.Elem}[{axesText e.Axes}]"
    let units = match e.Units with Some u -> $" <{u}>" | None -> ""
    let identity =
        match e.ContentHash with
        | Some h -> $"identity: content sha256:{h.Substring(0, min 12 h.Length)}"
        | None -> $"identity: {e.Identity} (size + mtime observed at run)"
    $"{e.Name}: {e.Provider} {e.Path}#{e.Variable} -- {shape}{units}; {identity}"

let entryJson (e: InputEntry) : string =
    let units = match e.Units with Some u -> jsonString u | None -> "null"
    let hash = match e.ContentHash with Some h -> $",\"content_hash\":{jsonString h}" | None -> ""
    $"{{\"name\":{jsonString e.Name},\"provider\":{jsonString e.Provider},\"path\":{jsonString e.Path},\"variable\":{jsonString e.Variable},\"elem\":{jsonString e.Elem},\"axes\":{axesJson e.Axes},\"storage\":{jsonString e.Storage},\"units\":{units},\"identity\":{jsonString e.Identity}{hash}}}"

let renderJson (entries: InputEntry list) : string =
    "[" + (entries |> List.map entryJson |> String.concat ",") + "]"

// ---------------------------------------------------------------------------
// Emission: the baked table + the at-exit writer (blade_run_record.hpp)
// ---------------------------------------------------------------------------

/// A C string literal: printable ASCII verbatim, everything else (control
/// bytes and every byte of a non-ASCII character, UTF-8 encoded) as a
/// three-digit octal escape, so the emitted text stays ASCII and a digit
/// after an escape cannot extend it.
let private cString (s: string) : string =
    let sb = StringBuilder()
    sb.Append('"') |> ignore
    for b in System.Text.Encoding.UTF8.GetBytes s do
        match char b with
        | '"' -> sb.Append("\\\"") |> ignore
        | '\\' -> sb.Append("\\\\") |> ignore
        | '\n' -> sb.Append("\\n") |> ignore
        | '\r' -> sb.Append("\\r") |> ignore
        | '\t' -> sb.Append("\\t") |> ignore
        | c when c < ' ' || c > '~' -> sb.Append('\\').Append(System.Convert.ToString(int b, 8).PadLeft(3, '0')) |> ignore
        | c -> sb.Append(c) |> ignore
    sb.Append('"') |> ignore
    sb.ToString()

/// The file-scope lines codegen splices after the includes (and after the
/// MPI globals, whose rank the writer reads through a pointer): the manifest
/// table and the static `AtExit` whose destructor writes the record.
/// `rankExpr` is `&__blade_mpi_rank` for an MPI program and `nullptr`
/// otherwise; `usesRng` marks a program that draws from the `rand` module.
let cppLines (program: string) (bladeVersion: string) (usesRng: bool) (rankExpr: string) (entries: InputEntry list) : string list =
    let rows =
        entries |> List.map (fun e ->
            let units = match e.Units with Some u -> u | None -> ""
            let hash = match e.ContentHash with Some h -> h | None -> ""
            "    { " + ([ e.Name; e.Provider; e.Path; e.Variable; e.Elem; axesJson e.Axes; e.Storage; units; e.Identity; hash ]
                        |> List.map cString |> String.concat ", ") + " },")
    [ "// Run record (BLADE_RUN_RECORD): the input manifest this program was compiled against,"
      "// written with what the run observed when the process exits (blade_run_record.hpp)."
      "static const blade_rr::Input __blade_rr_inputs[] = {" ]
    @ rows
    @ [ "    { nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr }"
        "};"
        (let rng = if usesRng then "true" else "false"
         let prog = cString program
         let ver = cString bladeVersion
         $"static blade_rr::AtExit __blade_rr_at_exit{{ {prog}, {ver}, {rng}, __blade_rr_inputs, {entries.Length}, {rankExpr} }};") ]
