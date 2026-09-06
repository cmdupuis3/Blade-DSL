// The user-facing CLI commands: compile / run / check / emit, the REPL loop,
// and the shared failure-reporting helpers. Split out of Cli.fs, which keeps
// only dispatch; CliSelfTests.fs drives these commands under `blade test`.
module Blade.CliCommands

open System
open System.IO
open Blade.Build
open Blade.Lowering

let compilerVersion = "0.20.0"

let printUsage () =
    printfn "Blade Compiler v%s" compilerVersion
    printfn ""
    printfn "Usage: blade <command> [options]"
    printfn ""
    printfn "Commands:"
    printfn "  compile <file.edgi> [-o output]   Compile to C++ (and optionally to executable)"
    printfn "  run <file.edgi>                   Compile and run a Blade program"
    printfn "  run <file.edgi> --mpi <N>         ... with `where mpi` kernels decomposed across"
    printfn "                                    N ranks (compiled -lmsmpi, run under mpiexec)"
    printfn "  run <file.edgi> --memcheck        ... as a Debug+AddressSanitizer build that prints"
    printfn "                                    a BLADE-MEMCHECK allocator-stats line on stderr"
    printfn "                                    at exit (Windows: needs a vcvars64 environment;"
    printfn "                                    equivalently set BLADE_MEMCHECK=1)"
    printfn "  check <file.edgi>                 Type-check only (no code generation)"
    printfn "  plan <file.edgi> [--json]         Type-check and lower, then list every optimization"
    printfn "                                    decision the cost-only layer took (rule, subject,"
    printfn "                                    applied/declined with reason, evidence)"
    printfn "  doctor [--json]                   Report native-toolchain health: g++/OpenMP core"
    printfn "                                    (real compile+run), BLAS/LAPACK tier, NetCDF, MPI,"
    printfn "                                    CUDA, setup tools; exit 0 iff the g++ core works"
    printfn "  setup [--blas=MODE]               Bootstrap the environment: verify a BLAS config"
    printfn "                                    with the doctor probes, persist it to"
    printfn "                                    blade.toolchain.json; --blas=source builds OpenBLAS"
    printfn "                                    from the deps.json pin ('blade setup --help')"
    printfn "  ide check --json <file.edgi>      Type-check and emit JSON diagnostics + binding types"
    printfn "                                    (machine-readable, for editor tooling)"
    printfn "  ide serve                         Persistent editor daemon: NDJSON check requests on"
    printfn "                                    stdin, one JSON response line each on stdout"
    printfn "                                    (tier fast = typecheck; full = + monomorphization)"
    printfn "  ide surface                       Dump the language surface -- keywords, operators,"
    printfn "                                    intrinsics, builtins, scalar types, builtin calls,"
    printfn "                                    BLxxxx registry -- as one JSON line (the generator"
    printfn "                                    for protocol/surface.json)"
    printfn "  repl                              Interactive session: each input recompiles and"
    printfn "                                    re-runs the accumulated program, printing new values"
    printfn "                                    with types; bare expressions evaluate and echo"
    printfn "  emit <file.edgi> [-o output.cpp]  Emit C++ source without compiling"
    printfn "  test                              Run full test suite (IR + C++ + run)"
    printfn "  test --omp                        ... including the OpenMP thread-coverage block"
    printfn "  test --cuda                       ... including the CUDA kernel block"
    printfn "                                    (Windows: run from the x64 Native Tools prompt"
    printfn "                                     so nvcc finds cl.exe)"
    printfn "  test --mpi                        ... including the MPI decomposition block"
    printfn "                                    (needs mingw msmpi + the MS-MPI runtime)"
    printfn "  test --timing                     ... including the differential timing block (slow)"
    printfn "  test --interp                     ... including the interpreter differential block (slow)"
    printfn "  test --diff-oracle                ... including the pinned-oracle differential block"
    printfn "                                    (skips cleanly without ./oracle/Blade.exe)"
    printfn "                                    (the --omp/--cuda/--timing/--mpi/--interp/"
    printfn "                                     --diff-oracle flags combine)"
    printfn "  test --ir-only                    Run IR-only tests (fast, no C++ compilation)"
    printfn "  test alloc                        Run C++ allocation-layout tests (contiguity/cardinality)"
    printfn "  test omp-pragma                   Run the OpenMP pragma-emission block standalone"
    printfn "  test omp-coverage                 Run the OpenMP thread-coverage block standalone"
    printfn "  test omp-reduce                   Run the comm-licensed parallel-reduction block standalone"
    printfn "  test linalg                       Run the blade_linalg dispatch-emission block standalone"
    printfn "                                    (+ the BLAS tier-resolution unit block)"
    printfn "  test doctor                       Run the `blade doctor` structural pins standalone"
    printfn "  test setup                        Run the `blade setup` parse/persist pins standalone"
    printfn "  test lapack                       Run the blade_lapack eigensolver-dispatch block standalone"
    printfn "  test multifile                    Run the cross-module (multi-file) corpus standalone"
    printfn "  test module-resolve               Run the file-based module resolver + units.SI block"
    printfn "  test shapespec                    Run the shape-specialization reach block standalone"
    printfn "  test flat-path                    Run the flat-elementwise reach block standalone"
    printfn "  test cuda                         Run the CUDA kernel block standalone"
    printfn "  test mpi                          Run the MPI decomposition block standalone"
    printfn "  test netcdf                       Run the NetCDF provider block (needs libnetcdf + sample.nc)"
    printfn "  test zarr                         Run the Zarr provider block (hermetic; g++ for the e2e parts)"
    printfn "  test timing                       Run the differential timing block standalone"
    printfn "  test strict-pins                  Run the --strict-pins CLI gate block standalone"
    printfn "  test surfacing                    Run the warning-surfacing block standalone"
    printfn "  test surface                      Run the `ide surface` block standalone (renderer,"
    printfn "                                    serve arm, committed protocol/ snapshots)"
    printfn "  test ide-serve                    Run the `ide serve` NDJSON protocol block standalone"
    printfn "  test ide-eval                     Run the notebook session-eval block standalone"
    printfn "  test ide-cells                    Run the notebook checkCells assembly block standalone"
    printfn "  test ide-references               Run the `references[]` navigation payload block standalone"
    printfn "  test gr-render                    Run the GR renderPlot block standalone (frame bytes,"
    printfn "                                    worker protocol; the live-GR case skips without one)"
    printfn "  test diff-oracle [category]       Diff printed values against the pinned ./oracle build"
    printfn "  test interp [category]            Diff the tree-walking interpreter against the compiled binary"
    printfn ""
    printfn "Options:"
    printfn "  -o <path>      Output file path"
    printfn "  --verbose      Show IR and generated C++"
    printfn "  --strict-pins  Fail the build on unpinned confirm-and-pin deductions"
    printfn "                 (BL4010, normally warnings). For CI: forces the pin"
    printfn "                 decision into source. check / compile / emit / run."
    printfn "  --no-cache     Always run g++, ignoring the content-addressed"
    printfn "                 executable cache (%%LOCALAPPDATA%%\\Blade\\exe-cache;"
    printfn "                 BLADE_EXE_CACHE=0 / a path override it). compile / run / test."
    printfn "  --help         Show this help"
    printfn ""
    printfn "Examples:"
    printfn "  blade run myprogram.edgi"
    printfn "  blade emit myprogram.edgi -o myprogram.cpp"
    printfn "  blade compile myprogram.edgi -o myprogram"
    printfn "  blade check myprogram.edgi --strict-pins"
    printfn "  blade test"
    printfn "  blade test --omp --cuda --timing"
    printfn "  blade test --llvm-backend  (the ordinary suite, corpus driven through the LLVM"
    printfn "                              lane and judged against its own EXPECT pins; no g++)"
    printfn "  blade test llvm            (BLADE_LLVM lane vs the C++ lane; standalone only)"
    printfn "  blade test llvm all        (the same differential over EVERY corpus category,"
    printfn "                              reporting what fraction of it the lane can emit)"
    printfn "  blade test llvm-bench      (codegen-speed and runtime tables for both lanes)"

/// Strict-pins mode. Confirm-and-pin SUGGESTIONS (BL4010) are warnings by
/// default: the deduction proposes, storage stays DENSE, nothing changes
/// until the user pins the annotation in source. `--strict-pins` promotes
/// every outstanding suggestion to a build ERROR, so a CI build fails on an
/// unpinned deduction that would change storage.
///
/// Reads the `PinSuggestions` side-channel `typeCheck` fills (structured
/// (message, kernel span) twin of the plain-string warnings) and renders each
/// through the ordinary diagnostic renderer, so a strict failure looks like
/// any other `error[BL4010]:`. None when strict mode is off or nothing outstanding.
let private strictPinFailure (strictPins: bool) (useColor: bool)
                             (sm: Blade.Diagnostics.SourceMap option) : string option =
    if not strictPins then None
    else
        match Blade.TypeCheckIde.PinSuggestions.get () |> List.distinct with
        | [] -> None
        | suggestions ->
            let ds =
                suggestions |> List.map (fun (msg, span) ->
                    Blade.Diagnostics.mkError "BL4010" Blade.Diagnostics.PhConstraints span msg
                    |> Blade.Diagnostics.withNote
                        "--strict-pins: an unpinned deduction that would change storage fails the build. Add the pin shown above, or drop --strict-pins for the default dense-until-pinned behavior.")
            Some (Blade.Diagnostics.Render.renderAll useColor sm ds)

/// EVERY non-zero exit must carry a diagnostic the user can act on: a compiler
/// that refuses a program and says nothing is indistinguishable from a crashed
/// one. The failure strings the compile driver hands back are
/// `Render.renderAll` output, and `renderAll` of the EMPTY list is the EMPTY
/// STRING -- a rendered diagnostic is never empty, since `render` always emits
/// a severity header, so the empty string can only mean the list was empty.
/// Printing that as-is exits 1 with a blank stderr and no .cpp, which is
/// exactly the mute failure this guard exists to prevent.
///
/// A non-empty message is printed byte for byte, as before; only the
/// nothing-to-say case changes, and it becomes a BL9001 internal error.
let internal reportFailure (message: string) : int =
    if String.IsNullOrWhiteSpace message then
        let useColor = not Console.IsErrorRedirected
        let d =
            Blade.Diagnostics.Codes.ice
                "the compilation failed but recorded no diagnostic, so there is nothing to \
                 report about your program. Re-run with --verbose and please report the input"
        eprintfn "%s" (Blade.Diagnostics.Render.render useColor None d)
    else
        eprintfn "%s" message
    1

/// Argument-shape failure: the usage text goes to stdout as before, but the
/// REASON goes to stderr, so a caller that only captures stderr (every sweep
/// script) sees why the invocation was rejected instead of an empty exit 1.
let internal usageFailure (reason: string) : int =
    eprintfn "error: %s" reason
    printUsage ()
    1

/// The front half EVERY back end shares: parse, resolve file imports,
/// typecheck, lower, validate. Split out of `compileFile` so the C++ emitter
/// and the BLADE_LLVM lane consume ONE front-end pass -- a lane that refuses
/// and falls back must not make the program pay for two.
///
/// `mark` is the phase-timing sink; the caller owns the stopwatch so the marks
/// of both halves land on one timeline.
let private frontEndToIR (filePath: string) (strictPins: bool) (mark: string -> unit)
        : Result<Blade.IR.IRProgram * Blade.Diagnostics.SourceMap, string> =
    // Env-gated compiler perf counters (BLADE_PERF_COUNTERS=1); refreshed
    // here so the gate is live before the front end runs. See
    // docs/plan-compile-speed.md Stage 5.
    Blade.PerfCounters.refresh ()
    let source = File.ReadAllText(filePath)
    // Errors come back as coded, spanned Diagnostics, rendered rustc-style with source snippets.
    let useColor = not Console.IsErrorRedirected
    // `lowerFileDiag` resolves file-based imports (`import units.SI` ->
    // stdlib/units/SI.blade) first and lowers the whole set. With nothing
    // to resolve it IS `lowerDiag (Some filePath) source`.
    let lowered = lowerFileDiag filePath source
    mark "frontend-total(lowerFileDiag)"
    match lowered with
    | Error ds, sm ->
        // A file with a hard error has still EARNED every warning the checker produced before it failed.
        printTypeCheckWarnings useColor (Some sm) false
        Error (Blade.Diagnostics.Render.renderAll useColor (Some sm) ds)
    | Ok (ir, _), sm ->
        // Strict mode fails here, before codegen: the pin suggestions
        // REPLACE their warning twins (which are therefore not printed).
        match strictPinFailure strictPins useColor (Some sm) with
        | Some rendered -> Error rendered
        | None ->
        printTypeCheckWarnings useColor (Some sm) false
        let validated = IRValidate.validateIR ir
        mark "validateIR"
        match validated with
        | Error errs ->
            let ds =
                errs |> List.map (fun s ->
                    Blade.Diagnostics.mkError "BL6001" Blade.Diagnostics.PhIRValidate Blade.Ast.noSpan s)
            Error (Blade.Diagnostics.Render.renderAll useColor (Some sm) ds)
        | Ok ir -> Ok (ir, sm)

/// The C++ back half: IRProgram -> translation unit, plus the two
/// refusal channels codegen reports through.
let private cppBackendOfIR (ir: Blade.IR.IRProgram) (testName: string) (sm: Blade.Diagnostics.SourceMap)
                           (verbose: bool) (timing: bool) (swAll: System.Diagnostics.Stopwatch)
                           (mark: string -> unit) : Result<string * string list, string> =
    let useColor = not Console.IsErrorRedirected
    let (cppCode, warnings) = CodeGen.genSelfContainedProgramFromIR ir testName
    mark "codegen"
    // A shape that reached codegen with no arm for it. Refuse HERE,
    // as a coded Blade diagnostic spanned at the declaration --
    // handing the emitted `BLADE_CODEGEN_ERROR_...` placeholder to
    // g++ instead reports a Blade back-end gap as an undeclared
    // C++ identifier. Deliberate codegen refusals are unaffected:
    // they keep their `#error` guard and their own wording.
    match CodeGen.takeUnhandledIRNodeDiagnostics () with
    | (_ :: _) as ds ->
        Error (Blade.Diagnostics.Render.renderAll useColor (Some sm) ds)
    | [] ->
    // DELIBERATE refusals (BL7004): the same messages the emitted
    // `#error` directives carry, reported as coded diagnostics
    // before g++ ever runs. Gated on the generated source actually
    // carrying a marker -- a rendered-then-discarded refusal
    // records a message but splices nothing, and must not fail a
    // program whose translation unit is clean.
    match CodeGen.takeCodegenRefusalDiagnostics cppCode with
    | (_ :: _) as ds ->
        Error (Blade.Diagnostics.Render.renderAll useColor (Some sm) ds)
    | [] ->
    mark "post-codegen-scans"
    if timing then
        eprintfn "[phase] compileFile total: %d ms (cpp %d chars)" swAll.ElapsedMilliseconds cppCode.Length
    if Blade.PerfCounters.enabled then
        // The IR walk is measurement-only, hence inside the gate.
        Blade.PerfCounters.noteIRNodes (IR.countProgramNodes ir)
        Blade.PerfCounters.report ()
    if verbose then
        for w in warnings do
            eprintfn "[Warning] %s" w
    else
        // A `where cuda` kernel that fell back to the host prints
        // WITHOUT --verbose: device emission is an explicit opt-in
        // (`--cuda` / the CUDA test phase), so "you asked and did not
        // get it" is the one codegen warning the user is entitled to
        // see unprompted. Every other warning keeps its --verbose gate,
        // and outside cuda emit mode this list is empty.
        for w in warnings do
            if w.StartsWith "[cuda] " then eprintfn "warning: %s" w
    Ok (cppCode, warnings)

/// Env-gated phase timing (BLADE_PHASE_TIMING=1); see docs/plan-compile-speed.md.
/// Returns the whole-run stopwatch, the per-phase `mark`, and the gate itself,
/// so front end and back half share ONE timeline however they are combined.
let private phaseTimers () =
    let timing = phaseTimingEnabled ()
    let swAll = System.Diagnostics.Stopwatch.StartNew()
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let mark name =
        if timing then
            eprintfn "[phase] %s: %d ms" name sw.ElapsedMilliseconds
        sw.Restart()
    (timing, swAll, mark)

/// Compile a .blade file to C++ source. The C++ lane specifically: `blade
/// test` and every in-process harness reach the back end through here, and
/// they are not affected by BLADE_LLVM. The routing fork lives in
/// `compileArtifact` below.
let compileFile (filePath: string) (verbose: bool) (strictPins: bool) : Result<string * string list, string> =
    if not (File.Exists filePath) then
        Error $"File not found: {filePath}"
    else
        let testName = Path.GetFileNameWithoutExtension(filePath)
        let (timing, swAll, mark) = phaseTimers ()
        match frontEndToIR filePath strictPins mark with
        | Error e -> Error e
        | Ok (ir, sm) -> cppBackendOfIR ir testName sm verbose timing swAll mark

/// What a back end produced for one program: a C++ translation unit (with its
/// codegen warnings) or the LLVM lane's textual `.ll`.
type BackendArtifact =
    | CppArtifact of source: string * warnings: string list
    | LlvmArtifact of ll: string

/// Compile a .blade file through whichever back end the environment selects.
///
/// BLADE_LLVM UNSET IS TODAY'S BEHAVIOR, BYTE FOR BYTE: the fork below is not
/// even consulted. With the gate on, the LLVM lane gets first refusal on the
/// SAME IRProgram the C++ emitter would have consumed -- one front-end pass
/// either way -- and any refusal prints exactly one notice and hands the
/// program to the C++ lane unchanged. The lane can therefore only be
/// all-correct or absent, which is what lets it grow against a byte-pinned
/// corpus without ever half-compiling anything.
let compileArtifact (filePath: string) (verbose: bool) (strictPins: bool) : Result<BackendArtifact, string> =
    if not (File.Exists filePath) then
        Error $"File not found: {filePath}"
    else
        let testName = Path.GetFileNameWithoutExtension(filePath)
        let (timing, swAll, mark) = phaseTimers ()
        match frontEndToIR filePath strictPins mark with
        | Error e -> Error e
        | Ok (ir, sm) ->
            let toCpp () =
                cppBackendOfIR ir testName sm verbose timing swAll mark
                |> Result.map CppArtifact
            let fallback (reason: string) =
                eprintfn "[blade] llvm lane refused: %s; falling back to C++" reason
                toCpp ()
            if not (Build.llvmEnabled ()) then toCpp ()
            else
                match Build.resolveClang () with
                | None -> fallback "no clang found (set BLADE_LLVM_CLANG)"
                | Some _ ->
                    match EmitLlvm.tryEmitProgramNamed testName ir with
                    | Error reason -> fallback reason
                    | Ok ll ->
                        mark "emit-llvm"
                        Ok (LlvmArtifact ll)

/// Place a produced executable at the caller's requested path (or leave it
/// where the toolchain put it), and report it under --verbose. Shared by both
/// back ends' arms of `compileToExe`.
let private placeExecutable (outputPath: string option) (verbose: bool) (exePath: string) : string =
    let finalPath =
        match outputPath with
        | Some out ->
            let outFull = Path.GetFullPath(out)
            if exePath <> outFull then
                try File.Copy(exePath, outFull, true) with _ -> ()
            outFull
        | None -> exePath
    if verbose then
        eprintfn "[Compile] %s" finalPath
    finalPath

/// Compile a .edgi file to an executable
let compileToExe (filePath: string) (outputPath: string option) (verbose: bool) (strictPins: bool) : Result<string, string> =
    match compileArtifact filePath verbose strictPins with
    | Error e -> Error e
    | Ok (LlvmArtifact ll) ->
        // The LLVM lane's whole back half: write the .ll where the .cpp would
        // have gone, deploy the C shim beside it (the link input), and let
        // clang do IR -> executable in one step.
        let baseName = Path.GetFileNameWithoutExtension(filePath)
        let dir = Path.GetDirectoryName(Path.GetFullPath(filePath))
        let dir = if String.IsNullOrEmpty dir then "." else dir
        let llFile = Path.Combine(dir, baseName + ".ll")
        File.WriteAllText(llFile, ll)
        let shimPath = Path.Combine(dir, EmitLlvm.shimFileName)
        let shimObjPath = Path.Combine(dir, Path.GetFileNameWithoutExtension EmitLlvm.shimFileName + Platforms.objExtension)
        // Same cleanup rule the C++ lane applies to its deployed headers:
        // remove only what THIS compile created, so a directory that already
        // held a shim (a scratch dir building many programs) keeps it.
        let shimWasAbsent = not (File.Exists shimPath)
        let shimObjWasAbsent = not (File.Exists shimObjPath)
        EmitLlvm.deployShim dir
        if verbose then
            eprintfn "[Emit] %s" llFile
        (match Build.compileLlvmProgram llFile dir with
         | Error e -> Error $"Compilation failed:\n{e}"
         | Ok exePath ->
             let finalPath = placeExecutable outputPath verbose exePath
             // verbose keeps the intermediates so the .ll can be inspected or
             // recompiled by hand.
             if not verbose then
                 try File.Delete(llFile) with _ -> ()
                 if shimWasAbsent then (try File.Delete(shimPath) with _ -> ())
                 if shimObjWasAbsent then (try File.Delete(shimObjPath) with _ -> ())
             Ok finalPath)
    | Ok (CppArtifact (cppCode, warnings)) ->
        let baseName = Path.GetFileNameWithoutExtension(filePath)
        let dir = Path.GetDirectoryName(Path.GetFullPath(filePath))
        let dir = if String.IsNullOrEmpty dir then "." else dir
        // Infer backend from generated source: device kernels -> .cu + nvcc.
        let backendReq = inferBackendReq cppCode
        let ext = match backendReq with RequiresCuda -> ".cu" | RequiresMpi | CpuOnly -> ".cpp"
        let cppFile = Path.Combine(dir, baseName + ext)
        File.WriteAllText(cppFile, cppCode)
        // `blade run --cuda`: `where cuda` device kernels are collected
        // SEPARATELY from the host source (cudaKernelDefsCell), so
        // inferBackendReq -- which greps the host half -- cannot see them.
        // Write the .cu beside the host .cpp and split-compile (nvcc for the
        // .cu, host compiler for the .cpp, then link), exactly as the CUDA
        // test harness does. Without this, the host half's extern "C" launch
        // declarations dangle and the link fails.
        let cudaSplitFile =
            match CodeGen.getCudaFileContent () with
            | Some cu when ext = ".cpp" ->
                let cuFile = Path.Combine(dir, baseName + "_kernels.cu")
                File.WriteAllText(cuFile, cu)
                Some cuFile
            | _ -> None
        // Runtime headers are #include'd with plain quotes and no -I, so they
        // must sit next to the .cpp; record which ones we create so cleanup removes only our copies.
        let deployedHeaders =
            CodeGen.runtimeHeaderNames
            |> List.map (fun name -> Path.Combine(dir, name))
            |> List.filter (fun path -> not (File.Exists path))
        CodeGen.deployRuntimeHeaders dir
        if verbose then
            eprintfn "[Emit] %s" cppFile
        match (match cudaSplitFile with
               | Some cuFile -> compileCudaSplit cuFile cppFile dir
               // cppCode is the exact text just written to cppFile; handing it
               // over spares the backend sniffs a read-back of what we wrote.
               | None -> compileForBackendSource (Some cppCode) capabilities.Value backendReq cppFile dir) with
        | Error e ->
            Error $"Compilation failed:\n{e}"
        | Ok exePath ->
            let finalPath = placeExecutable outputPath verbose exePath
            // verbose keeps the intermediates so the source can be inspected/recompiled.
            if not verbose then
                try File.Delete(cppFile) with _ -> ()
                for h in deployedHeaders do
                    try File.Delete(h) with _ -> ()
            Ok finalPath

/// Run a .edgi file: compile and execute. `mpiRanks = Some n` switches on the
/// MPI emit gate (decomposed kernels + Init/Finalize + rank-0 printing),
/// links -lmsmpi, launches under `mpiexec -n n`. None = serial path.
let runFile (filePath: string) (verbose: bool) (mpiRanks: int option) (strictPins: bool) : int =
    match mpiRanks with
    | None ->
        match compileToExe filePath None verbose strictPins with
        | Error e -> reportFailure e
        | Ok exePath ->
            match runExecutable exePath with
            | Error e ->
                eprintfn "Runtime error: %s" e
                1
            | Ok (exitCode, output) ->
                printf "%s" output
                exitCode
    | Some ranks ->
        CodeGen.setMpiEmitMode true
        try
            match compileToExe filePath None verbose strictPins with
            | Error e -> reportFailure e
            | Ok exePath ->
                match runExecutableMpi ranks exePath with
                | Error e ->
                    eprintfn "Runtime error: %s" e
                    1
                | Ok (exitCode, output) ->
                    printf "%s" output
                    exitCode
        finally
            CodeGen.setMpiEmitMode false

// Interactive REPL (`blade repl`): `blade run` semantics give REPL behavior
// for free, since every top-level binding prints its value. The REPL
// accumulates a session program in a temp file; each submitted snippet
// re-compiles and re-runs the WHOLE session, but echoes ONLY the value of the
// snippet's LAST top-level binding. Earlier bindings, and the synthetic
// `__`-internal bindings a `ppl.dist`/module call expands into, stay hidden.
// Rebinding a top-level name replaces the earlier definition in place
// (duplicate lets are a C++ redeclaration error) so downstream snippets still see it.
//
// A bare EXPRESSION snippet (`a`, `a + 1`) gets wrapped in a transient
// binding (`let it = <expr>`), run, echoed, and discarded, so the session
// stays untouched and repeating it echoes again. A bare identifier naming a
// session FUNCTION echoes its signature from the typechecker alone.
//
// Output lines are type-annotated by an in-process parse+typecheck(+lower,
// for HM-monomorphized value types) of the same source -- see ReplTypes. The
// compiled session runs with the REPL process's own cwd, so relative data
// paths resolve where the user launched the REPL, not in the session temp dir.

/// Run a compiled session exe with an explicit working directory, capturing
/// stdout/stderr separately (runExecutable pins cwd to the exe's dir, which
/// would break relative data paths for REPL sessions).
let private runExeIn (cwd: string) (exeFile: string) : Result<int * string * string, string> =
    try
        let psi = System.Diagnostics.ProcessStartInfo(Path.GetFullPath exeFile)
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.UseShellExecute <- false
        psi.CreateNoWindow <- true
        psi.WorkingDirectory <- cwd
        use proc = System.Diagnostics.Process.Start(psi)
        let stdoutTask = Blade.Runtime.readToEndOffPool proc.StandardOutput
        let stderrTask = Blade.Runtime.readToEndOffPool proc.StandardError
        if proc.WaitForExit(60000) then
            Ok (proc.ExitCode, stdoutTask.Result, stderrTask.Result)
        else
            (try proc.Kill() with _ -> ())
            Error "Execution timed out after 60s"
    with ex ->
        Error $"Execution exception: {ex.Message}"

/// The engine `blade repl` drives, shared with `ide serve`'s notebook lane:
/// the accumulating snippet list, rebind-in-place splicing, interp-first
/// evaluation and the ReplTypes annotation map all live in src/ReplSession.fs
/// (it has to compile before IdeServe.fs). What stays HERE is the interactive
/// front end -- prompts, classification branches, and every printed string.
module RS = Blade.ReplSession
module ReplTypes = Blade.ReplSession.ReplTypes

/// The g++ fallback lane, handed to the engine because `compileToExe` sits on
/// top of Build.fs and is unreachable from a file that compiles this early.
/// The two failure messages are composed exactly as the REPL has always
/// printed them, so the caller only has to add `[snippet not kept]`.
/// The RENDER lane's two halves (IdeServe's `render` command): build a source
/// into an executable and hand back its PATH, and run an executable already
/// built. Split apart because the whole point of that lane is to do the second
/// WITHOUT the first -- a camera change re-runs a binary it compiled once.
let internal renderCompileLane (srcPath: string) : Result<string, string> =
    compileToExe srcPath None false false

let internal renderRunLane (cwd: string) (exeFile: string) : Result<int * string * string, string> =
    runExeIn cwd exeFile

let internal compiledReplLane (srcPath: string) (cwd: string) : Result<int * string * string, string> =
    match compileToExe srcPath None false false with
    | Error e -> Error e
    | Ok exePath ->
        match runExeIn cwd exePath with
        | Error e -> Error $"Runtime error: {e}"
        | Ok triple -> Ok triple

let replLoop () : int =
    printfn "Blade REPL (v%s) -- each submission echoes its last binding's (typed) value." compilerVersion
    printfn "A bare expression (e.g. `a`, `a + 1`) evaluates and echoes without joining the session."
    printfn "Commands: :reset (clear session)  :show (print session)  :quit"
    printfn "Multi-line: unbalanced brackets continue on the next line, or use :paste ... :end"
    let engine = RS.ReplSession(Directory.GetCurrentDirectory())
    let session = engine.Snippets
    let mutable lastLines : string[] = [||]

    /// Evaluate `candidate` and echo ONLY `targetName`'s value line, type-
    /// annotated. Every earlier user binding, and every synthetic
    /// `__`-internal binding, stays hidden.
    ///
    /// The evaluation itself (one lowering, interpreter first, g++ only where
    /// the interpreter falls short) is the engine's; this is the PRINTING half
    /// of the old compileRunEcho, and every stream it writes to -- rendered
    /// diagnostics, the fallback notice, the annotated echo, `[snippet not
    /// kept]` -- is in the same order it always was.
    ///
    /// `transient` is the synthetic name a bare expression was wrapped in
    /// (stripped in display), else None. Returns Some (lines, printedCount,
    /// info) on a clean exit, or None if the snippet must not be kept.
    let compileRunEcho (candidate: ResizeArray<string>) (targetName: string option) (transient: string option)
        : (string[] * int * Map<string, ReplTypes.Info>) option =
        let useColor = not Console.IsErrorRedirected
        match engine.EvalCandidate(candidate, targetName, None, eprintfn "%s") with
        | RS.CandidateRejected (ds, sm) ->
            // Front-end / validate rejection: same diagnostics as compileToExe's Error arm.
            eprintfn "%s" (Blade.Diagnostics.Render.renderAll useColor (Some sm) ds)
            eprintfn "[snippet not kept]"
            None
        | RS.CandidateFailed msg ->
            eprintfn "%s" msg
            eprintfn "[snippet not kept]"
            None
        | RS.CandidateRan r ->
            // Interpreter lane: surface the same TypeCheck warnings the g++
            // path prints for itself, in the same place (before the echo).
            (match r.Warnings with
             | [] -> ()
             | ds -> eprintfn "%s" (Blade.Diagnostics.Render.renderAll useColor None ds))
            // Display frames (Blade-REPL/docs/display-frames.md section 4) go to
            // stdout, whole lines at column 0, BEFORE the echo and therefore
            // before the next prompt -- which is the frame terminator the
            // editor's REPL client scans for. This loop otherwise prints only
            // the echoed binding, so without this arm the pty channel would
            // carry no frames at all.
            //
            // Every accumulated snippet re-runs on every submission, so an
            // earlier submission's frames re-appear here. That is the spec's
            // section 10 replay contract: `meta.id`s are stable across re-runs,
            // so the panel merges them into the plots it already has. A human
            // reading a raw `blade repl` sees one long line per frame; the
            // editor strips them before the terminal ever shows them.
            for l in r.Lines do
                if l.StartsWith Blade.Display.Frame.Sentinel then printfn "%s" l
            let mutable printed = 0
            match r.Echo with
            | Some l -> printfn "%s" (ReplTypes.annotate r.Info transient l); printed <- 1
            | None -> ()
            if r.Stderr.Trim() <> "" then eprintfn "%s" (r.Stderr.Trim())
            if r.ExitCode = 0 then Some (r.Lines, printed, r.Info)
            else
                eprintfn "[exit %d -- snippet not kept]" r.ExitCode
                None

    let evaluate (snippet: string) =
        let trimmed = snippet.Trim()
        if trimmed = "" then () else
        if RS.declRe.IsMatch (RS.classifyTarget trimmed) then
            // Declaration: rebinding replaces the earlier definition IN PLACE
            // so later snippets referencing the name see the update.
            let (candidate, _) = engine.DeclarationCandidate trimmed
            // The submission's "return value" is its LAST top-level binding
            // (a :paste block may declare several); echo only that one.
            let lastTarget =
                trimmed.Replace("\r\n", "\n").Split('\n')
                |> Array.choose RS.bindingName
                |> Array.tryLast
            match compileRunEcho candidate lastTarget None with
            | None -> ()
            | Some (lines, printed, info) ->
                let mutable printed = printed
                // A final function/type binding produces no run output -- echo
                // its signature (abstract unless inference bound it concrete).
                match lastTarget with
                | Some name when printed = 0 ->
                    match Map.tryFind name info with
                    | Some (ReplTypes.RFunc s) ->
                        printfn "%s\n\t%s" name s
                        printed <- printed + 1
                    | _ -> ()
                | _ -> ()
                if printed = 0 then printfn "(ok)"   // defs print nothing new
                engine.Commit candidate
                lastLines <- lines
        elif RS.assignRe.IsMatch (RS.classifyTarget trimmed) then
            // Reassignment (`b = b + 1`, `b += 1`, etc.): the engine wraps it
            // in a hidden binding whose value IS the ExprAssign, and KEEPS the
            // wrapper so the mutation persists.
            let (candidate, _, _, _) = engine.AssignmentCandidate trimmed
            let root = (RS.assignRe.Match (RS.classifyTarget trimmed)).Groups.[1].Value
            match compileRunEcho candidate (Some root) None with
            | None -> ()                                    // static/unknown/etc -> not kept
            | Some (lines, printed, _) ->
                if printed = 0 then printfn "(ok)"          // e.g. array reassign isn't auto-printed
                engine.Commit candidate
                lastLines <- lines
        else
            // Bare expression: `blade run` semantics only print top-level
            // BINDINGS, so wrap the expression in a transient one, run, and
            // echo its value WITHOUT keeping it, so re-entering the same
            // expression echoes again rather than diffing to silence.
            let curInfo = lazy (ReplTypes.sessionInfo (String.concat "\n\n" session + "\n"))
            let asFuncName =
                RS.bareIdentifier trimmed
                |> Option.bind (fun n ->
                    match Map.tryFind n curInfo.Value with
                    | Some (ReplTypes.RFunc s) -> Some (n, s)
                    | _ -> None)
            match asFuncName with
            | Some (n, s) ->
                // A function can't be let-bound just to echo it; print its
                // signature straight from the typechecker.
                printfn "%s\n\t%s" n s
            | None ->
                let (candidate, _, transient, _) = engine.ExpressionCandidate trimmed
                match compileRunEcho candidate (Some transient) (Some transient) with
                | None -> ()
                | Some (_, printed, info) ->
                    if printed = 0 then
                        // Nothing printable (unit, deferred computation,
                        // function value): show the type alone if known.
                        match Map.tryFind transient info with
                        | Some (ReplTypes.RVal t) -> printfn "\t%s" (Blade.Ide.ppType t)
                        | _ -> printfn "(ok)"

    let bracketBalance (text: string) =
        let mutable d = 0
        for c in text do
            match c with
            | '(' | '[' | '{' -> d <- d + 1
            | ')' | ']' | '}' -> d <- d - 1
            | _ -> ()
        d

    let buffer = ResizeArray<string>()
    let mutable pasteMode = false
    let mutable finished = false
    while not finished do
        Console.Write(if pasteMode || buffer.Count > 0 then "  ... " else "blade> ")
        Console.Out.Flush()
        // Strip BOM/zero-width characters some clients prepend to piped input
        // (a U+FEFF-prefixed `let` otherwise defeats rebind detection).
        let readLine () =
            match Console.ReadLine() with
            | null -> null
            | l -> l.Replace("\uFEFF", "").Replace("\u200B", "")
        match readLine () with
        | null -> finished <- true
        | line when pasteMode ->
            if line.Trim() = ":end" then
                pasteMode <- false
                evaluate (String.concat "\n" buffer)
                buffer.Clear()
            else buffer.Add line
        | line when buffer.Count = 0 && line.Trim() = ":paste" -> pasteMode <- true
        | line when buffer.Count = 0 && (line.Trim() = ":quit" || line.Trim() = ":q") -> finished <- true
        | line when buffer.Count = 0 && line.Trim() = ":reset" ->
            engine.Reset()
            lastLines <- [||]
            printfn "(session cleared)"
        | line when buffer.Count = 0 && line.Trim() = ":show" ->
            // Hide the synthetic `let __assign... = <reassignment>` wrappers the assignment path appends.
            let visible =
                session
                |> Seq.filter (fun s -> RS.bindingName s |> Option.forall (fun n -> not (n.StartsWith "__assign")))
                |> List.ofSeq
            if List.isEmpty visible then printfn "(empty session)"
            else printfn "%s" (String.concat "\n\n" visible)
        | line ->
            buffer.Add line
            if bracketBalance (String.concat "\n" buffer) <= 0 then
                evaluate (String.concat "\n" buffer)
                buffer.Clear()
    engine.Cleanup()
    0

/// End-to-end CLI smoke test: compile and run a one-line .edgi from a FRESH
/// temp directory -- the only block exercising the user-facing path from a
/// bare directory (other runners pre-deploy runtime headers, masking a compileToExe that forgets to).

/// `blade plan <file>`: the optimization DECISION RECORD (plan-fortran-
/// killer-2.md section 3 step 5). Installs a Blade.Effects.Decisions
/// collector, runs the same parse -> typecheck -> lower pipeline `emit`
/// runs (every cost-only pass fires during lowering), and prints one line
/// per decision: rule and version, subject, source position when the pass
/// had one, applied or declined with the first reason, and the evidence
/// the pass discharged. Diagnostic data only -- nothing here is a licence
/// to branch on an optimizer outcome from Blade source.
let planFile (filePath: string) (json: bool) : int =
    if not (File.Exists filePath) then
        reportFailure $"File not found: {filePath}"
    else
        let source = File.ReadAllText(filePath)
        let useColor = not Console.IsErrorRedirected
        Blade.Effects.Decisions.start ()
        match Blade.Lowering.lowerFileDiag filePath source with
        | Error ds, sm ->
            Blade.Effects.Decisions.drain () |> ignore
            reportFailure (Blade.Diagnostics.Render.renderAll useColor (Some sm) ds)
        | Ok _, _ ->
            let ds = Blade.Effects.Decisions.drain ()
            if json then
                printfn "%s" (Blade.Effects.Decisions.renderJson filePath ds)
            else
                printfn "plan: %s -- %d optimization decision(s)" filePath ds.Length
                for d in ds do
                    printfn "  %s" (Blade.Effects.Decisions.render d)
            0

let checkFile (filePath: string) (strictPins: bool) : int =
    if not (File.Exists filePath) then
        reportFailure $"File not found: {filePath}"
    else
        let source = File.ReadAllText(filePath)
        let useColor = not Console.IsErrorRedirected
        // Same shape as compileFile's `lowerFileDiag`: parse the entry ONCE,
        // resolve file-based imports from that AST, then check the whole set as
        // ONE program. With nothing to resolve, the entry parse is the only
        // parse and `sources` is [(filePath, source)] -- byte for byte the
        // pre-module behavior, including the SourceMap key.
        match Blade.Parser.parseProgramWithFile (Some filePath) source with
        | Error e ->
            let sm = Blade.Diagnostics.SourceMap.ofSources [ filePath, source ]
            let d = Blade.Parser.diagnosticOfParseError (Some filePath) e
            reportFailure (Blade.Diagnostics.Render.render useColor (Some sm) d)
        | Ok entryProgram ->
        let resolution =
            match entryProgram.Modules with
            | [ m ] -> Blade.ModuleResolve.resolveParsedEntry filePath source m
            | _ -> Blade.ModuleResolve.resolveEntry filePath source
        let sources =
            match resolution.Errors, resolution.Files with
            | [], [ _single ] -> [ filePath, source ]
            | _, files -> Blade.ModuleResolve.sourcesOf files
        let sm = Blade.Diagnostics.SourceMap.ofSources sources
        if not (List.isEmpty resolution.Errors) then
            reportFailure (Blade.Diagnostics.Render.renderAll useColor (Some sm) resolution.Errors)
        else
        match (match resolution.Files with
               | [ _single ] -> Ok entryProgram
               | files -> Blade.ModuleResolve.parseResolvedFiles files) with
        | Error d ->
            reportFailure (Blade.Diagnostics.Render.render useColor (Some sm) d)
        | Ok program ->
            match Blade.TypeCheck.typeCheck program with
            | Error errors ->
                // S1: warnings earned before the error are printed, not dropped.
                printTypeCheckWarnings useColor (Some sm) false
                let ds = errors |> List.map Blade.TypeEnv.diagnosticOfCompileError
                reportFailure (Blade.Diagnostics.Render.renderAll useColor (Some sm) ds)
            | Ok _ ->
                match strictPinFailure strictPins useColor (Some sm) with
                | Some rendered ->
                    // Strict mode: the pin suggestions ARE the failure. Their
                    // warning twins are dropped (same dedup rule as `blade ide
                    // check`) so each is reported exactly once, as an error;
                    // the twins are BL4010 by construction so filtering on the code is exact.
                    printTypeCheckWarnings useColor (Some sm) true
                    reportFailure rendered
                | None ->
                    printTypeCheckWarnings useColor (Some sm) false
                    printfn "OK"
                    0

/// Emit back-end source to file or stdout: C++ normally, textual LLVM IR when
/// the BLADE_LLVM lane took the program.
let emitFile (filePath: string) (outputPath: string option) (verbose: bool) (strictPins: bool) : int =
    match compileArtifact filePath verbose strictPins with
    | Error e -> reportFailure e
    | Ok artifact ->
        let text = match artifact with CppArtifact (src, _) -> src | LlvmArtifact ll -> ll
        match outputPath with
        | Some outPath ->
            // The write and the runtime deploy are the two ways an emit can
            // fail for a reason that is not about the program (read-only path,
            // full disk, a concurrent writer holding the destination). Name the
            // file rather than letting the top-level boundary report a bare
            // .NET message with no path in it.
            try
                File.WriteAllText(outPath, text)
                let outDir = Path.GetDirectoryName(Path.GetFullPath(outPath))
                let outDir = if String.IsNullOrEmpty outDir then "." else outDir
                // Ship the runtime next to the emitted source so the file
                // compiles as-is with no -I: the C++ headers for a .cpp,
                // the C shim for a .ll.
                (match artifact with
                 | CppArtifact _ -> CodeGen.deployRuntimeHeaders outDir
                 | LlvmArtifact _ -> EmitLlvm.deployShim outDir)
                if verbose then
                    eprintfn "[Emit] %s" outPath
                0
            with ex ->
                reportFailure $"Failed to write {outPath}: {ex.Message}"
        | None ->
            printf "%s" text
            0

/// `--strict-pins` regression block. A corpus entry can't express a FLAG's
/// behavior, so this drives checkFile and compileFile (which compile/emit/run
/// funnel through) against a temp file in-process. Uses corpus twins
/// functions/026 (unpinned -> suggestion) / functions/029 (pin applied), inline so the test survives corpus renumbering.
