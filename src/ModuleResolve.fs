// Module resolution: dotted import name -> .blade file on disk, plus the
// transitive closure of an entry file's imports in a deterministic dependency
// order.
//
// This is the FILE-BASED module path -- the same mechanism the multi-file
// corpus already exercises through `lowerMultiSource`, but driven from a real
// entry file instead of a hand-assembled source list. It is NOT the builtin
// pseudo-module path: `import math` / `ml` / `ppl` / `rand` / `sgs` /
// `spectra` / `ad` and the provider modules (`netcdf`, `zarr`, `csv`) are
// hardcoded name matches inside their own elaborators, so this resolver
// deliberately steps over them (`isBuiltinModule`) and never looks for a file.
//
// Search order for `import a.b.c` (mapped to the relative path a/b/c.blade):
//
//   1. The stdlib roots, in this order:
//        a. $BLADE_STDLIB, when set and it exists (the override);
//        b. <dir of the running Blade.exe>/stdlib, then that directory's
//           parents up to `probeDepth` levels -- so a dev tree finds
//           <repo>/stdlib from bin/Debug/net10.0 and bin/Release/net10.0 alike,
//           and a deployed tree finds the copy the .fsproj drops next to the
//           binary;
//        c. the same upward probe from the current working directory.
//   2. The IMPORTING file's own directory (so `import mylib.helpers` beside
//      the entry file finds ./mylib/helpers.blade).
//   3. The ENTRY file's directory, when different from (2), so a nested user
//      module can still name a sibling of the program that pulled it in.
//
// Every candidate the search touched is named in the not-found error, because
// "module not found" without the search path is unactionable.
//
// Compiles after ProviderStatics.fs (it asks the provider registry which names
// are provider modules) and before Ide.fs / Lowering.fs / Cli.fs, its three
// consumers.
module Blade.ModuleResolve

open System
open System.IO
open Blade.Ast
open Blade.Diagnostics

/// One resolved compilation unit.
type ResolvedFile = {
    /// Absolute path on disk. The file's IDENTITY: cycle detection, duplicate
    /// detection and the visited set all key on it.
    Path: string
    /// The path STAMPED into this file's spans (`Span.File`), and therefore the
    /// key its source must have in a SourceMap. Equal to `Path` for every file
    /// the resolver parsed itself; for an entry whose caller had already parsed
    /// it, whatever string the caller stamped -- typically the path the user
    /// typed, which is what its diagnostics have always rendered.
    Stamp: string
    /// File contents, read exactly once.
    Source: string
    /// Dotted name the file DECLARES (`module units.SI` -> "units.SI"). The
    /// entry file usually declares nothing, which parses as "Main".
    Declared: string
    /// The module parsed while resolving, kept so the pipeline does not parse
    /// every file a second time. `None` only when the file failed to parse --
    /// the caller re-parses it to obtain the error at its own span.
    Parsed: ModuleDecl option
}

/// Builtin pseudo-modules: names that resolve through a hardcoded match in an
/// elaborator rather than through a file. Kept in sync with the
/// `DeclImport (["x"], _)` matches in Grad.fs / MLElaborate.fs /
/// PplElaborate.fs / MathElaborate.fs / RandElaborate.fs / SgsElaborate.fs /
/// SpectraElaborate.fs / DisplayElaborate.fs.
let private elaboratorModules =
    set [ "ad"; "display"; "math"; "ml"; "ppl"; "rand"; "sgs"; "spectra" ]

/// Provider module names. The registry is the source of truth, but it is only
/// populated by `ProviderStatics.install ()` (which typeCheck runs), so the
/// literal set is the floor -- resolution happens BEFORE typecheck.
let private providerModulesFallback = set [ "csv"; "icechunk"; "netcdf"; "zarr" ]

/// Does this dotted import name belong to a builtin pseudo-module (and so
/// must NOT be looked for on disk)?
let isBuiltinModule (qname: QualifiedName) : bool =
    match qname with
    | [] -> true
    | head :: _ when head = "Providers" -> true   // the retired provider spelling; TypeCheck reports it
    | [ single ] ->
        elaboratorModules.Contains single
        || providerModulesFallback.Contains single
        || (Blade.ProviderRegistry.tryFind single).IsSome
    | _ -> false

// Stdlib discovery

/// How many parent directories the stdlib probe walks up from a starting
/// point. bin/<config>/net10.0 -> repo root is 3; 5 leaves room for a
/// publish layout without turning the probe into a filesystem crawl.
let private probeDepth = 5

/// `<dir>/stdlib`, `<dir>/../stdlib`, ... up to probeDepth levels.
let private upwardStdlibCandidates (start: string) : string list =
    let rec go (d: string) (n: int) acc =
        if n < 0 || String.IsNullOrEmpty d then List.rev acc
        else
            let acc = Path.Combine(d, "stdlib") :: acc
            let parent = try Path.GetDirectoryName d with _ -> null
            if isNull parent || parent = d then List.rev acc else go parent (n - 1) acc
    go start probeDepth []

/// The same probe, narrowed to candidates whose PARENT is a Blade CHECKOUT --
/// a directory holding Blade.fsproj. This is the SOURCE stdlib, as opposed to
/// the build-output copy the .fsproj drops beside the binary.
///
/// It exists because those two are not interchangeable and the copy used to
/// win. `upwardStdlibCandidates` is nearest-first, so from
/// bin/Release/net10.0/Blade.exe the copy at bin/Release/net10.0/stdlib shadowed
/// <repo>/stdlib three levels up -- which meant a stdlib edit did nothing until
/// a rebuild, and (because MSBuild's PreserveNewest compares timestamps) a
/// RESTORED-OLDER file did nothing even then, surfacing later as a type error
/// pointing into bin/ at a file the user never wrote.
///
/// tests/Corpus.fs and Cli.fs's `artifact` helper had already settled this the
/// other way for the corpus and protocol/*.json -- source first, deployed copy
/// as the fallback -- and this brings the stdlib into line.
///
/// The Blade.fsproj gate is what keeps that safe. A plain source-first order
/// would have to trust an unqualified upward walk, and five levels up from an
/// arbitrary working directory is enough to collide with an unrelated project's
/// `stdlib/` folder. Requiring the marker means these candidates cannot appear
/// outside a checkout, so a DEPLOYED tree produces none of them and its search
/// order is exactly what it was.
let private upwardRepoStdlibCandidates (start: string) : string list =
    upwardStdlibCandidates start
    |> List.filter (fun p ->
        match (try Path.GetDirectoryName p with _ -> null) with
        | null -> false
        | d -> try File.Exists (Path.Combine(d, "Blade.fsproj")) with _ -> false)

/// Memo for `stdlibRoots`, keyed by the two inputs that can move under a
/// running process: $BLADE_STDLIB and the working directory. Keyed rather than
/// `Lazy` precisely so a caller (the test block, an embedder) can point
/// BLADE_STDLIB somewhere else and be believed; a plain `Lazy` would freeze the
/// first answer for the life of the process. A benign race recomputes, it never
/// returns a stale list.
let mutable private rootsMemo : (string * string * string list) option = None

/// The stdlib roots that actually exist, in search order.
let stdlibRoots () : string list =
    let envVal =
        match Environment.GetEnvironmentVariable "BLADE_STDLIB" with
        | null -> "" | v -> v
    let cwd = try Directory.GetCurrentDirectory() with _ -> ""
    match rootsMemo with
    | Some (e, c, roots) when e = envVal && c = cwd -> roots
    | _ ->
        let envRoot = if envVal = "" then [] else [ envVal ]
        // Checkout-gated probes FIRST (working directory, then the binary's own
        // tree), so a dev build reads <repo>/stdlib rather than the copy sitting
        // beside it. Both are needed and neither subsumes the other: the IDE
        // spawns the compiler with the USER'S project as the working directory,
        // so only the binary-rooted probe rescues that case; a `dotnet run` from
        // the checkout with a stale binary elsewhere is rescued only by the
        // cwd-rooted one. In a deployed tree both are empty (no Blade.fsproj)
        // and the two ungated probes below are the whole search, unchanged.
        let roots =
            (envRoot
             @ upwardRepoStdlibCandidates cwd
             @ upwardRepoStdlibCandidates AppContext.BaseDirectory
             @ upwardStdlibCandidates AppContext.BaseDirectory
             @ upwardStdlibCandidates cwd)
            |> List.map (fun p -> try Path.GetFullPath p with _ -> p)
            |> List.distinct
            |> List.filter (fun p -> try Directory.Exists p with _ -> false)
        rootsMemo <- Some (envVal, cwd, roots)
        roots

/// `units.SI` -> `units\SI.blade` (platform separator).
let relativePathOf (qname: QualifiedName) : string =
    Path.Combine(Array.ofList qname) + ".blade"

/// Every directory the search for `qname` will look in, in order. Exposed so
/// the not-found diagnostic and the search itself cannot drift apart.
let searchDirs (importerDir: string) (entryDir: string) : string list =
    (stdlibRoots () @ [ importerDir; entryDir ])
    |> List.map (fun p -> try Path.GetFullPath p with _ -> p)
    |> List.distinct

/// First existing candidate for `qname`, plus the full candidate list.
let private findModuleFile (importerDir: string) (entryDir: string) (qname: QualifiedName)
    : string option * string list =
    let rel = relativePathOf qname
    let candidates = searchDirs importerDir entryDir |> List.map (fun d -> Path.Combine(d, rel))
    let hit = candidates |> List.tryFind (fun p -> try File.Exists p with _ -> false)
    (hit, candidates)

// Import discovery

/// Top-level imports of a parsed module, each with the span of its `import`
/// declaration (so a resolution failure points at the line that caused it).
let importsOf (m: ModuleDecl) : (QualifiedName * Span) list =
    m.Decls
    |> List.choose (fun d ->
        match d.Value with
        | DeclImport (qname, _) -> Some (qname, d.Span)
        | _ -> None)

/// Parse one file for its declared module name and its import list, KEEPING
/// the module: resolution used to throw this AST away and the pipeline parsed
/// every file a second time. A file that does not parse contributes NOTHING
/// here rather than failing resolution: the caller re-parses it and reports
/// the parse error once, at its own span, with the source map already built.
let private scanFile (path: string) (source: string) : string * (QualifiedName * Span) list * ModuleDecl option =
    match Blade.Parser.parseProgramWithFile (Some path) source with
    | Ok { Modules = [ m ] } -> (String.concat "." m.Name, importsOf m, Some m)
    | _ -> ("", [], None)

// Diagnostics

let private notFoundDiag (dotted: string) (span: Span) (candidates: string list) =
    let listed = candidates |> List.map (sprintf "  %s") |> String.concat "\n"
    mkError "BL2004" PhResolve span $"module '{dotted}' not found"
    |> withNote $"searched:\n{listed}"
    |> withNote "set BLADE_STDLIB to point at a stdlib directory, or place the module beside the file that imports it"

let private cycleDiag (chain: string list) (span: Span) =
    mkError "BL2005" PhResolve span
        ($"""import cycle: {(String.concat " -> " chain)}""")
    |> withNote "modules are compiled in dependency order, which a cycle makes impossible; break the cycle by moving the shared declarations into a third module"

let private duplicateDiag (dotted: string) (pathA: string) (pathB: string) (span: Span) =
    mkError "BL2006" PhResolve span
        $"module '{dotted}' is declared by two files"
    |> withNote $"first:  {pathA}"
    |> withNote $"second: {pathB}"

/// A resolved file whose `module` header disagrees with the name it was
/// imported under. Worth its own message because the import would otherwise
/// fail much later, as an unbound-name cascade: exports are keyed by the
/// DECLARED name, so `import a.b` only ever binds a file that says `module a.b`.
let private mismatchDiag (dotted: string) (declared: string) (path: string) (span: Span) =
    mkError "BL2006" PhResolve span
        $"module '{dotted}' resolved to a file that declares 'module {declared}'"
    |> withNote $"file: {path}"
    |> withNote $"rename the header to `module {dotted}`, or import it as `{declared}`"

// Resolution

/// What a resolution produced. `Files` is populated even when `Errors` is not:
/// every file the walk managed to READ is in there, which is what the caller
/// needs to build a SourceMap that can render the failing import's snippet.
type Resolution = {
    /// Dependency order, ENTRY LAST. Partial when `Errors` is non-empty.
    Files: ResolvedFile list
    /// Empty iff resolution succeeded.
    Errors: Diagnostic list
}

/// The transitive closure of `entryPath`'s imports, in dependency order with
/// the ENTRY LAST -- exactly the order `TypeCheck.checkProgram` needs, since
/// it accumulates one export per module as it goes and an importer must be
/// checked after everything it imports.
///
/// A single-element `Files` means nothing resolved to a file (the common case:
/// no imports at all, or only builtin pseudo-modules), and callers are
/// expected to take their existing single-file path unchanged.
///
/// `preScanned` lets a caller that has ALREADY parsed a file hand over the
/// module instead of paying for a second parse -- the driver and IDE paths,
/// which parse the entry buffer first (with their OWN file stamp, carried
/// along as `ResolvedFile.Stamp`) and would otherwise re-parse it. Empty for
/// callers holding no AST.
let resolveEntryWith (preScanned: Map<string, string * ModuleDecl>)
                     (entryPath: string) (entrySource: string) : Resolution =
    // Provider names are a resolution input, and only install () puts them in
    // the registry. Idempotent, and typeCheck runs it again later anyway.
    (try Blade.ProviderStatics.install () with _ -> ())
    let entryFull = try Path.GetFullPath entryPath with _ -> entryPath
    let dirOf (p: string) =
        let d = try Path.GetDirectoryName p with _ -> null
        if String.IsNullOrEmpty d then "." else d
    let entryDir = dirOf entryFull

    let errors = ResizeArray<Diagnostic>()
    // Declared module name -> the file that declared it (duplicate detection).
    let declaredBy = System.Collections.Generic.Dictionary<string, string>()
    // Absolute path -> the module name that file's header declares. Filled
    // BEFORE recursing, so the header/import-name agreement check can read it
    // even for a file the walk is still inside.
    let declaredIn = System.Collections.Generic.Dictionary<string, string>()
    // Absolute path -> resolution state. Grey = on the current DFS stack.
    let state = System.Collections.Generic.Dictionary<string, int>()   // 1 = grey, 2 = black
    // The grey stack, outermost first: (path, human label). Only the labels
    // reach the user, and only when a cycle closes.
    let stack = ResizeArray<string * string>()
    let ordered = ResizeArray<ResolvedFile>()

    let readSource (path: string) =
        if path = entryFull then entrySource else File.ReadAllText path

    let rec visit (path: string) (label: string) (via: Span) =
        match state.TryGetValue path with
        | true, 1 ->
            // Grey: the walk is already inside this file, so the stack from
            // its first entry onwards IS the cycle. Closing it with `label`
            // again makes the loop visible in the message.
            let names =
                match stack |> Seq.tryFindIndex (fun (p, _) -> p = path) with
                | Some i -> [ for k in i .. stack.Count - 1 -> snd stack.[k] ] @ [ label ]
                | None -> [ label; label ]
            errors.Add(cycleDiag names via)
        | true, _ -> ()   // black: already visited, already in `ordered`
        | _ ->
            state.[path] <- 1
            stack.Add((path, label))
            let source =
                try readSource path
                with ex ->
                    errors.Add(mkError "BL2004" PhResolve via
                                   $"cannot read module file '{path}': {ex.Message}")
                    ""
            let (stamp, declared, imports, parsed) =
                match Map.tryFind path preScanned with
                | Some (stamp, m) -> (stamp, String.concat "." m.Name, importsOf m, Some m)
                | None ->
                    let (declared, imports, parsed) = scanFile path source
                    (path, declared, imports, parsed)
            declaredIn.[path] <- declared
            if declared <> "" then
                match declaredBy.TryGetValue declared with
                | true, other when other <> path -> errors.Add(duplicateDiag declared other path via)
                | _ -> declaredBy.[declared] <- path
            let importerDir = dirOf path
            for (qname, span) in imports do
                if not (isBuiltinModule qname) then
                    let dotted = String.concat "." qname
                    match findModuleFile importerDir entryDir qname with
                    | (None, candidates) -> errors.Add(notFoundDiag dotted span candidates)
                    | (Some hit, _) ->
                        let full = try Path.GetFullPath hit with _ -> hit
                        visit full dotted span
                        // The header has to agree with the import name, or the
                        // export lookup in checkProgram simply misses.
                        match declaredIn.TryGetValue full with
                        | true, declaredThere when declaredThere <> "" && declaredThere <> dotted ->
                            errors.Add(mismatchDiag dotted declaredThere full span)
                        | _ -> ()
            stack.RemoveAt(stack.Count - 1)
            state.[path] <- 2
            ordered.Add { Path = path; Stamp = stamp; Source = source
                          Declared = declared; Parsed = parsed }

    visit entryFull (Path.GetFileName entryFull) noSpan
    { Files = List.ofSeq ordered; Errors = errors |> List.ofSeq |> List.distinct }

/// The ordinary entry point: parse everything, including the entry file.
let resolveEntry (entryPath: string) (entrySource: string) : Resolution =
    resolveEntryWith Map.empty entryPath entrySource

/// `resolveEntry` for a caller holding the entry file's already-parsed module
/// (the compile driver, the IDE). Identical result, one parse cheaper. The
/// entry's spans keep the caller's stamp -- `entryPath` as it was written, not
/// its absolutized form -- so its diagnostics render exactly as they do on the
/// single-file path, where the caller's parse is the only one there is.
let resolveParsedEntry (entryPath: string) (entrySource: string) (entry: ModuleDecl) : Resolution =
    let key = try Path.GetFullPath entryPath with _ -> entryPath
    resolveEntryWith (Map.ofList [ key, (entryPath, entry) ]) entryPath entrySource

/// The (stamp, source) pairs the lowering entry points consume: keyed on the
/// path each file's SPANS carry, which is what a SourceMap lookup matches on.
let sourcesOf (files: ResolvedFile list) : (string * string) list =
    files |> List.map (fun f -> (f.Stamp, f.Source))

/// SourceMap over everything a resolution read, so a diagnostic pointing into
/// a MEMBER file still renders with its snippet.
let sourceMapOf (r: Resolution) : Blade.Diagnostics.SourceMap =
    Blade.Diagnostics.SourceMap.ofSources (sourcesOf r.Files)

/// Parse a resolved set into ONE `Program`, in the given order.
///
/// Deliberately NOT `Parser.parseMultiSource`: that entry point renames a
/// module whose header is absent (parsed as the default `Main`) after its
/// FILE NAME, which is right for the corpus harness -- where the "file name"
/// is the module name -- and wrong here, where it is an absolute path. Parsing
/// each file with `parseProgramWithFile` keeps the header as the only source
/// of a module's name, so an entry file with no `module` line stays `Main`
/// exactly as it does on the single-file path.
let parseResolved (sources: (string * string) list) : Result<Program, Diagnostic> =
    let rec go acc rest =
        match rest with
        | [] -> Ok { Modules = List.rev acc }
        | (path: string, src: string) :: tl ->
            match Blade.Parser.parseProgramWithFile (Some path) src with
            | Error e -> Error (Blade.Parser.diagnosticOfParseError (Some path) e)
            | Ok p -> go (List.rev p.Modules @ acc) tl
    go [] sources

/// `parseResolved` for a caller holding the resolution itself: every file that
/// parsed during resolution is REUSED rather than parsed again (which is every
/// file, on any path that gets as far as lowering). Only a file that failed to
/// parse is parsed again here -- once, to produce its error at its own span,
/// which is exactly what resolution deliberately swallowed.
let parseResolvedFiles (files: ResolvedFile list) : Result<Program, Diagnostic> =
    let rec go acc rest =
        match rest with
        | [] -> Ok { Modules = List.rev acc }
        | (f: ResolvedFile) :: tl ->
            match f.Parsed with
            | Some m -> go (m :: acc) tl
            | None ->
                match Blade.Parser.parseProgramWithFile (Some f.Stamp) f.Source with
                | Error e -> Error (Blade.Parser.diagnosticOfParseError (Some f.Stamp) e)
                | Ok p -> go (List.rev p.Modules @ acc) tl
    go [] files
