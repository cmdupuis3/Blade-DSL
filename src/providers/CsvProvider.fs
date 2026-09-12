// Blade-DSL CSV Provider: comma-separated text tables as compile-time-shaped array
// I/O. A CSV file is a single text file; metadata (shape, dtype) comes from parsing
// it at compile time, which also serves the static fold and the interpreter
// (ReadVarData). Runtime I/O is generated C++ using only <fstream>/<sstream> (no
// link-time dependency, unlike NetcdfProvider's libnetcdf).
//
// File model (sniffed off the first non-empty record, the same rule the C++ reader
// bakes in):
//   - Every first-row cell numeric -> MATRIX mode: R x C numbers, one 2-D var `data`
//     with plain (anonymous) Idx axes.
//   - Otherwise -> HEADERED mode: first row = column labels; `data`'s COLUMN axis is
//     a synthesized EnumIdx over the labels (`<binding>_cols`), selected by label:
//     `obs.vars.data[i, "temp"]`. Labels must be non-empty unique strings; an
//     all-numeric header is indistinguishable from a matrix and unsupported (rename
//     the column).
//
// Format rules, enforced identically here and in the emitted C++: delimiter is comma
// only, no quoting/escaping (any '"' is an error); LF and CRLF both accepted (one
// trailing '\r' stripped per line), a UTF-8 BOM on line 1 is stripped, one trailing
// newline tolerated; ragged rows, empty cells, and interior blank lines are errors
// named with a line number; data cells are numeric only (strings deferred --
// ProviderPayload is closed over floats/ints). Whole-table dtype: every cell an
// integer literal -> Int64, else Float64 (locale-independent; "nan"/"inf"/"-inf"
// accepted as float specials to round-trip C++ output).
//
// Writes (`c.write("out.csv", A)`, rank <= 2): no header row; rank-1 writes one value
// per line (reloads as R x 1), rank-2 writes comma rows. Floats print with 17
// significant digits and a forced decimal point so `2.0` never reloads as Int64.
module Blade.CsvProvider

open System
open System.IO
open Blade.IR
open Blade.Types

// Metadata model

type CsvShape =
    /// First row = column labels; Rows = data-row count (header excluded).
    | CsvTable of labels: string list * rows: int
    /// Headerless all-numeric grid.
    | CsvMatrix of rows: int * cols: int

type CsvFile = {
    Path: string
    Shape: CsvShape
    /// ETInt64 iff every data cell is an integer literal, else ETFloat64.
    Elem: ElemType
}

/// Column count regardless of mode.
let colCount (f: CsvFile) =
    match f.Shape with
    | CsvTable (labels, _) -> labels.Length
    | CsvMatrix (_, c) -> c

let rowCount (f: CsvFile) =
    match f.Shape with
    | CsvTable (_, r) -> r
    | CsvMatrix (r, _) -> r

// The one parser (metadata, fold payload, and interp reads all derive)

/// Integer-literal cell: optional sign, digits only. This decides Int64 vs
/// Float64 -- "1e5" and "1.0" are floats even though integral in value.
let private isIntCell (s: string) =
    let s = s.Trim()
    if s.Length = 0 then false
    else
        let body = if s.[0] = '+' || s.[0] = '-' then s.Substring 1 else s
        body.Length > 0 && body |> Seq.forall Char.IsDigit

/// Locale-independent float parse; accepts the C-locale specials the C++
/// writer produces ("nan"/"inf"/"-inf"), spelled differently by .NET.
let private tryParseFloat (s: string) : float option =
    let t = s.Trim()
    match t.ToLowerInvariant() with
    | "nan" | "+nan" | "-nan" -> Some nan
    | "inf" | "+inf" | "infinity" | "+infinity" -> Some infinity
    | "-inf" | "-infinity" -> Some (-infinity)
    | _ ->
        match Double.TryParse(t, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
        | true, v -> Some v
        | _ -> None

let private isNumericCell (s: string) = (tryParseFloat s).IsSome

/// Raw rectangular cells, validated against the format rules above; every
/// error names its 1-based line (BOM stripped, lone trailing blank dropped).
let parseCells (path: string) (text: string) : Result<string[][], string> =
    // NB: the char literal below is U+FEFF (the BOM) -- invisible in most editors.
    let text = if text.Length > 0 && text.[0] = '\uFEFF' then text.Substring 1 else text
    let rawLines = text.Split '\n' |> Array.map (fun l -> if l.EndsWith "\r" then l.Substring(0, l.Length - 1) else l)
    // One trailing newline => one trailing "" entry; tolerate exactly that.
    let lines =
        if rawLines.Length > 0 && rawLines.[rawLines.Length - 1] = "" then
            rawLines.[.. rawLines.Length - 2]
        else rawLines
    if lines.Length = 0 then Error $"'{path}' is empty"
    else
        let mutable err = None
        let cells =
            lines |> Array.mapi (fun i line ->
                let lineNo = i + 1
                if err.IsSome then [||]
                elif line = "" then
                    err <- Some $"blank line in '{path}' at line {lineNo}"; [||]
                elif line.Contains "\"" then
                    err <- Some $"quote character in '{path}' at line {lineNo} -- quoting/escaping is not supported (v1)"; [||]
                else
                    let row = line.Split ','
                    match row |> Array.tryFindIndex (fun c -> c.Trim() = "") with
                    | Some ci ->
                        err <- Some $"empty cell (column {ci + 1}) in '{path}' at line {lineNo}"; [||]
                    | None -> row |> Array.map (fun c -> c.Trim()))
        match err with
        | Some e -> Error e
        | None ->
            let width = cells.[0].Length
            match cells |> Array.tryFindIndex (fun r -> r.Length <> width) with
            | Some i ->
                Error $"ragged row in '{path}' at line {i + 1}: {cells.[i].Length} cells where line 1 has {width}"
            | None -> Ok cells

/// Parse + classify: sniffing rule, label validation, dtype inference.
/// Returns the metadata plus the full cell grid.
let parseFile (path: string) : Result<CsvFile * string[][], string> =
    if not (File.Exists path) then
        Error $"CSV file not found: '{path}' (resolved against cwd '{Directory.GetCurrentDirectory()}')"
    else
    parseCells path (File.ReadAllText path) |> Result.bind (fun cells ->
        let headered = not (cells.[0] |> Array.forall isNumericCell)
        let dataRows = if headered then cells.[1..] else cells
        if dataRows.Length = 0 then
            Error $"'{path}' has a header row but no data rows"
        else
            // Validate every data cell numeric; name the first offender.
            let mutable bad = None
            for i in 0 .. dataRows.Length - 1 do
                if bad.IsNone then
                    match dataRows.[i] |> Array.tryFindIndex (not << isNumericCell) with
                    | Some ci ->
                        let lineNo = (if headered then i + 2 else i + 1)
                        bad <- Some $"non-numeric cell '{dataRows.[i].[ci]}' (column {ci + 1}) in '{path}' at line {lineNo} -- string columns are not supported (v1)"
                    | None -> ()
            match bad with
            | Some e -> Error e
            | None ->
                let shapeResult =
                    if headered then
                        let labels = cells.[0] |> List.ofArray
                        let dup =
                            labels |> List.countBy id |> List.tryFind (fun (_, n) -> n > 1)
                        match dup with
                        | Some (l, _) -> Error $"duplicate column label '{l}' in '{path}'"
                        | None -> Ok (CsvTable (labels, dataRows.Length))
                    else
                        Ok (CsvMatrix (dataRows.Length, cells.[0].Length))
                shapeResult |> Result.map (fun shape ->
                    let elem =
                        if dataRows |> Array.forall (Array.forall isIntCell) then ETInt64 else ETFloat64
                    ({ Path = path; Shape = shape; Elem = elem }, cells))
    )

/// Metadata-only load; throws with path + cwd detail -- the message must
/// carry everything since the TypeCheck call site swallows exceptions
/// silently, leaving Lowering's uncaught call as the loud surface.
let loadMeta (path: string) : CsvFile =
    match parseFile path with
    | Ok (f, _) -> f
    | Error e -> failwith $"CSV load failed: {e}"

// Compile-time payload (static fold + interpreter dense reads)

/// The single 2-D var every CSV module exposes.
[<Literal>]
let DataVarName = "data"

let readVarData (path: string) (varName: string) : Result<Blade.ProviderRegistry.ProviderVarData, string> =
    if varName <> DataVarName then
        Error $"CSV file '{path}' has no variable '{varName}' (the only variable is '{DataVarName}')"
    else
        parseFile path |> Result.bind (fun (f, cells) ->
            let headered = match f.Shape with CsvTable _ -> true | CsvMatrix _ -> false
            let dataRows = if headered then cells.[1..] else cells
            let rows = dataRows.Length
            let cols = colCount f
            let payload =
                match f.Elem with
                | ETInt64 ->
                    let xs = Array.zeroCreate<int64> (rows * cols)
                    for r in 0 .. rows - 1 do
                        for c in 0 .. cols - 1 do
                            xs.[r * cols + c] <- Int64.Parse(dataRows.[r].[c], Globalization.CultureInfo.InvariantCulture)
                    Blade.ProviderRegistry.PInts xs
                | _ ->
                    let xs = Array.zeroCreate<float> (rows * cols)
                    for r in 0 .. rows - 1 do
                        for c in 0 .. cols - 1 do
                            xs.[r * cols + c] <-
                                match tryParseFloat dataRows.[r].[c] with
                                | Some v -> v
                                | None -> failwith "unreachable: cells validated numeric by parseFile"
                    Blade.ProviderRegistry.PFloats xs
            Ok { DimLengths = [rows; cols]; Payload = payload })

// Mapping to Blade IR types

/// Anonymous plain index (matrix axes and the table's row axis).
let private anonIdx (builder: IRBuilder) (extent: int64) : IRIndexType =
    { Id = builder.FreshId()
      Rank = 1
      Extent = IRLit (IRLitInt extent)
      Symmetry = SymNone
      Tag = None; IxKind = IxKPlain
      Kind = SDimension
      Dependencies = [] }

/// The synthesized column-EnumIdx tag for a headered load bound as `name`.
let colsTagName (moduleName: string) = $"{moduleName}_cols"

/// Compile-time metadata -> IRModule. One var `data` in a `<name>__vars`
/// struct (suffixed so several CSV loads in one program don't clobber each
/// other in TypeDefs -- registerProviderModule resolves the suffix).
/// Headered mode also emits an IRTDEnumIdx for the column axis so
/// string-literal column subscripts fold to ordinals at the indexing site.
let loadAsModule (builder: IRBuilder) (moduleName: string) (path: string) : IRModule =
    let f = loadMeta path
    let rows = int64 (match f.Shape with CsvTable (_, r) -> r | CsvMatrix (r, _) -> r)
    let cols = int64 (colCount f)
    let rowIdx = anonIdx builder rows
    let (colIdx, enumDefs) =
        match f.Shape with
        | CsvMatrix _ -> (anonIdx builder cols, [])
        | CsvTable (labels, _) ->
            let tag = colsTagName moduleName
            let idx =
                { Id = builder.FreshId()
                  Rank = 1
                  Extent = IRLit (IRLitInt cols)
                  Symmetry = SymNone
                  Tag = Some tag; IxKind = ixKindOfTag (Some tag)
                  Kind = SDimension
                  Dependencies = [] }
            let values = labels |> List.map EVString
            (idx, [IRTDEnumIdx (tag, idx, values)])
    let arrType = {
        ElemType = IRTScalar f.Elem
        IndexTypes = [rowIdx; colIdx]
        IsVirtual = false
        Identity = Some (AIDVariable DataVarName)
    }
    let varsStruct = IRTDStruct ($"{moduleName}__vars", [(DataVarName, mkArrayLike arrType)])
    {
        Name = moduleName
        Types = enumDefs @ [varsStruct]
        Functions = []
        Bindings = []
        StaticFunctionUsage = Map.empty
        ProviderReads = Map.empty
        ProviderWrites = Map.empty
        RandomInits = Map.empty
        CompoundInits = Map.empty
        SparseInits = Map.empty
        MutableArrayLets = Set.empty
        DerivedFuncOrigins = Map.empty
    }

// Fingerprint / version stamp (single-file provenance)

let fileFingerprint (path: string) : string =
    use sha = Security.Cryptography.SHA256.Create()
    sha.ComputeHash(File.ReadAllBytes path)
    |> Array.map (sprintf "%02x")
    |> String.concat ""

let fileVersionStamp (path: string) : int64 =
    try File.GetLastWriteTimeUtc(path).Ticks with _ -> 0L

// C++ code generation (pure std C++17: <fstream>, <sstream>)

module CppCsv =

    let private elemCppOf (t: IRType) : string =
        match t with
        | IRTScalar ETInt64 -> "long long"
        | IRTScalar ETInt32 -> "int"
        | IRTScalar ETFloat32 -> "float"
        | _ -> "double"

    /// C++ string literal for a path (forward slashes; backslashes normalized).
    let private cppPath (p: string) = p.Replace("\\", "/")

    let private csvExit (v: string) (msg: string) =
        $"{{ std::cerr << \"CSV error: {msg}\" << std::endl; std::exit(1); }}"

    /// Emits the parse-and-fill block: opens the file, re-applies the format
    /// rules (BOM/CRLF/quotes/ragged/blank), validates the baked shape, and
    /// fills `<v>_flat` (row-major R x C). strtod/strtoll are locale-
    /// sensitive, but generated programs never call setlocale, so "C" is guaranteed.
    let private genParseFill (path: string) (v: string) (elemCpp: string) (isInt: bool)
                             (headered: bool) (rows: int64) (cols: int64) : string list =
        let p = cppPath path
        let convert =
            if isInt then
                [ $"            long long {v}_val = std::strtoll({v}_cs, &{v}_end, 10);" ]
            else
                [ $"            double {v}_val = std::strtod({v}_cs, &{v}_end);" ]
        [ $"""// Read {v} from CSV {p} ({rows} x {cols}{(if headered then ", headered" else "")})"""
          $"{elemCpp}* {v}_flat = new {elemCpp}[{rows * cols}];"
          "{"
          $"    std::ifstream {v}_in(\"{p}\");"
          $"""    if (!{v}_in) {(csvExit v $"cannot open '{p}'")}"""
          $"    std::string {v}_line;"
          $"    size_t {v}_row = 0, {v}_lineno = 0;"
          $"    while (std::getline({v}_in, {v}_line)) {{"
          $"        {v}_lineno++;"
          $"        if (!{v}_line.empty() && {v}_line.back() == '\\r') {v}_line.pop_back();"
          $"        if ({v}_lineno == 1 && {v}_line.size() >= 3 && (unsigned char){v}_line[0] == 0xEF && (unsigned char){v}_line[1] == 0xBB && (unsigned char){v}_line[2] == 0xBF) {v}_line.erase(0, 3);"
          // A blank line is legal only as the last line (trailing-newline
          // artifact); getline absorbs one trailing newline, so a blank
          // here means "\n\n" at EOF or an interior blank.
          sprintf "        if (%s_line.empty()) { if (%s_in.peek() == EOF) break; %s }" v v
              (csvExit v $"blank line in '{p}' at line \" << {v}_lineno << \"")
          sprintf "        if (%s_line.find('\"') != std::string::npos) %s" v
              (csvExit v $"quote character in '{p}' at line \" << {v}_lineno << \" -- quoting is not supported (v1)") ]
        @ (if headered then
            [ $"        if ({v}_lineno == 1) continue;  // header row (labels baked at compile time)" ]
           else [])
        @ [ $"""        if ({v}_row >= {rows}) {(csvExit v $"'{p}' has more data rows than the {rows} baked at compile time -- file changed since compilation?")}"""
            $"        size_t {v}_col = 0, {v}_pos = 0;"
            "        while (true) {"
            $"            size_t {v}_comma = {v}_line.find(',', {v}_pos);"
            $"            size_t {v}_len = ({v}_comma == std::string::npos ? {v}_line.size() : {v}_comma) - {v}_pos;"
            $"            std::string {v}_cell = {v}_line.substr({v}_pos, {v}_len);"
            $"""            if ({v}_col >= {cols}) {(csvExit v $"row at line \" << {v}_lineno << \" of '{p}' has more than {cols} cells")}"""
            $"            const char* {v}_cs = {v}_cell.c_str(); char* {v}_end = nullptr;" ]
        @ convert
        @ [ sprintf "            if (%s_end == %s_cs || *%s_end != '\\0') %s" v v v
                (csvExit v $"non-numeric cell '\" << {v}_cell << \"' in '{p}' at line \" << {v}_lineno << \"")
            $"            {v}_flat[{v}_row * {cols} + {v}_col] = ({elemCpp}){v}_val;"
            $"            {v}_col++;"
            $"            if ({v}_comma == std::string::npos) break;"
            $"            {v}_pos = {v}_comma + 1;"
            "        }"
            $"""        if ({v}_col != {cols}) {(csvExit v $"row at line \" << {v}_lineno << \" of '{p}' has \" << {v}_col << \" cells where {cols} were baked at compile time")}"""
            $"        {v}_row++;"
            "    }"
            $"""    if ({v}_row != {rows}) {(csvExit v $"'{p}' has \" << {v}_row << \" data rows where {rows} were baked at compile time -- file changed since compilation?")}"""
            "}" ]

    /// Dense reader: parse-and-fill into `<v>_flat`, then the standard
    /// materialization (extents, allocate<>, flat->nested copy, release),
    /// the same closing form as CppZarr.genReadVar.
    let genReadVar (path: string) (varName: string) (cppVarName: string) (arrType: IRArrayType) : string list =
        if varName <> DataVarName then
            failwith $"CSV codegen: variable '{varName}' not found in '{path}' (the only variable is '{DataVarName}')"
        let f = loadMeta path
        let headered = match f.Shape with CsvTable _ -> true | CsvMatrix _ -> false
        let rows = int64 (match f.Shape with CsvTable (_, r) -> r | CsvMatrix (r, _) -> r)
        let cols = int64 (colCount f)
        let v = cppVarName
        let elemCpp = elemCppOf arrType.ElemType
        let isInt = (f.Elem = ETInt64)
        let assemble = genParseFill path v elemCpp isInt headered rows cols
        let materialize =
            [ $"size_t {v}_extent_0 = {rows};"
              $"size_t {v}_extent_1 = {cols};"
              $"size_t {v}_extents[] = {{ {v}_extent_0, {v}_extent_1 }};"
              $"Array<{elemCpp}, 2> {v} = {{ allocate<typename promote<{elemCpp}, 2>::type, nullptr>({v}_extents), {v}_extents }};"
              $"for (size_t {v}_i0 = 0; {v}_i0 < {v}_extent_0; {v}_i0++) {{"
              $"    for (size_t {v}_i1 = 0; {v}_i1 < {v}_extent_1; {v}_i1++) {{"
              $"        {v}[{v}_i0][{v}_i1] = {v}_flat[{v}_i0 * {v}_extent_1 + {v}_i1];"
              "    }"
              "}"
              $"delete[] {v}_flat;" ]
        assemble @ materialize

    /// Dense writer: `<v>_flat` (populated by the write intercept) streamed
    /// out as comma rows. Rank-1 writes one value per line (re-loads as
    /// R x 1). Floats print at max_digits10 (17) with a forced decimal
    /// point so a whole-valued float column re-loads as Float64, not Int64.
    let genWriteVar (path: string) (varName: string) (cppVarName: string) (arrType: IRArrayType) (_dimNames: string list) : string list =
        let v = cppVarName
        let p = cppPath path
        arrType.IndexTypes |> List.iter (fun ix ->
            if ix.Symmetry <> SymNone || ix.Rank <> 1 then
                failwith $"CSV write of '{varName}': packed/compound index groups are not supported -- densify first")
        let litExtent (e: IRExpr) =
            match e with
            | IRLit (IRLitInt n) -> n
            | _ -> failwith $"CSV write of '{varName}' requires literal extents"
        let extents = arrType.IndexTypes |> List.map (fun ix -> litExtent ix.Extent)
        let (rows, cols) =
            match extents with
            | [r] -> (r, 1L)
            | [r; c] -> (r, c)
            | _ -> failwith $"CSV write of '{varName}': rank {extents.Length} is not supported (rank 1 or 2 only)"
        let elemCpp = elemCppOf arrType.ElemType
        let isInt = (elemCpp = "long long" || elemCpp = "int")
        let cellExpr =
            if isInt then
                [ $"        if ({v}_c) {v}_out << ',';"
                  $"        {v}_out << {v}_flat[{v}_r * {cols} + {v}_c];" ]
            else
                [ $"        std::ostringstream {v}_os;"
                  $"        {v}_os << std::setprecision(17) << {v}_flat[{v}_r * {cols} + {v}_c];"
                  $"        std::string {v}_s = {v}_os.str();"
                  $"        if ({v}_s.find('.') == std::string::npos && {v}_s.find('e') == std::string::npos && {v}_s.find('n') == std::string::npos && {v}_s.find('i') == std::string::npos) {v}_s += \".0\";"
                  $"        if ({v}_c) {v}_out << ',';"
                  $"        {v}_out << {v}_s;" ]
        [ $"// Write {varName} to CSV {p} ({rows} x {cols}, no header)"
          "{"
          $"    std::ofstream {v}_out(\"{p}\", std::ios::trunc);"
          $"""    if (!{v}_out) {(csvExit v $"cannot open '{p}' for writing")}"""
          $"    for (size_t {v}_r = 0; {v}_r < {rows}; {v}_r++) {{"
          $"      for (size_t {v}_c = 0; {v}_c < {cols}; {v}_c++) {{" ]
        @ cellExpr
        @ [ "      }"
            $"      {v}_out << '\\n';"
            "    }"
            $"""    if (!{v}_out.good()) {(csvExit v $"write failed for '{p}'")}"""
            "}" ]

    /// Required C++ includes for CSV I/O (std only -- no link flags).
    let genIncludes () : string list =
        [ "#include <fstream>"
          "#include <sstream>"
          "#include <string>"
          "#include <iostream>"
          "#include <iomanip>"
          "#include <cstdlib>" ]

// F#-side fixture writer (tests and programmatic file creation)

module CsvWrite =

    /// Exact-text control: caller supplies finished lines, written LF-terminated.
    let writeRaw (path: string) (lines: string list) : unit =
        File.WriteAllText(path, (lines |> String.concat "\n") + "\n")

    /// Headered table from string cells (caller formats numbers).
    let writeTable (path: string) (header: string list) (rows: string list list) : unit =
        writeRaw path ((String.concat "," header) :: (rows |> List.map (String.concat ",")))

    /// Headerless float matrix; round-trip ("R") formatting with a forced decimal point.
    let writeMatrix (path: string) (data: float[][]) : unit =
        let cell (x: float) =
            let s = x.ToString("R", Globalization.CultureInfo.InvariantCulture)
            if s.Contains "." || s.Contains "e" || s.Contains "E" || s.Contains "N" || s.Contains "I" then s
            else s + ".0"
        writeRaw path (data |> Array.toList |> List.map (fun row -> row |> Array.map cell |> String.concat ","))

/// The csv ProviderSpec (surface module name "csv").
let spec : Blade.ProviderRegistry.ProviderSpec = {
    Name = "csv"
    LoadAsModule = loadAsModule
    ReadVarData = readVarData
    GenReadVar = CppCsv.genReadVar
    GenReadPacked = None       // packed groups: not representable in CSV
    ReadWreathPool = None      // OrbIdx pools: likewise (CSV has no pool axis)
    GenReadCompoundVar = None  // load_compound: rejected loudly
    GenWriteVar = CppCsv.genWriteVar
    GenStreamOpen = None       // streaming: future arc (rejected loudly)
    GenStreamFiber = None
    GenStreamRowsOpen = None
    GenStreamRows = None
    StreamRowsBlock = None
    GenStreamWindow = None
    Includes = CppCsv.genIncludes
    VarDimNames = fun _ _ -> None  // CSV carries no dimension names
    Fingerprint = fileFingerprint
    VersionStamp = fileVersionStamp
    LinkNeeds = "none (pure std C++17)"
}
