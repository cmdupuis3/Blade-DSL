// Command-line interface: argument parsing and command dispatch only.
// Main.fs is the entry point; the commands live in CliCommands.fs and the
// CLI self-test blocks in CliSelfTests.fs.
module Blade.Cli

open System
open System.IO
open Blade.Tests.RunAll
open Blade.CliCommands
open Blade.CliSelfTests

let private dispatchInner (args: string[]) : int =
    // Share the compiler version with the test-harness output helpers so every
    // block header reads "(vX.Y.Z)" consistently, including standalone runs.
    Blade.Tests.TestHarness.version <- compilerVersion
    // The REPL engine compiles long before the toolchain driver it needs for
    // its g++ fallback; hand it over once, here, so every front end that
    // reaches an unsupported node from this process gets the same lane.
    Blade.ReplSession.installCompiledLane compiledReplLane
    // ...and the RENDER FAST PATH its own, for the same reason and by the same
    // route: IdeServe.fs is compiled before Build.fs, so it cannot name
    // compileToExe. It needs the EXE PATH back, not just the run's output, so
    // that a camera change can re-run a binary it already built.
    Blade.IdeServe.installRenderLane renderCompileLane renderRunLane
    // Reset per-invocation provider state ahead of every verb: without it, the
    // icechunk axis mint table and the IDE stores side-channel carry over
    // whatever a previous compilation in this process minted, so a checkout's
    // axis could print `lat#2` for no reason in the source. Must land here,
    // not between typecheck and lowering, or it orphans identities typecheck
    // just minted. `test` and `repl` compile many programs per process, so
    // they re-arm this at their own boundaries (`Interp.Repl.lowerSessionDiag`,
    // and where the icechunk block regenerates a fixture in place).
    Blade.ProviderRegistry.IdeStores.reset ()
    Blade.IcechunkProvider.resetAxisMint ()
    // `--strict-pins` is a build MODE, not a positional argument, and only
    // means anything for the four verbs that own a typecheck. Strip it from
    // the argv the verb patterns match on so every arm shape accepts it in
    // any position; left in place for every other verb (unknown argument there).
    let strictPinVerb =
        args.Length >= 1 &&
        (match args.[0] with
         | "check" | "compile" | "emit" | "run" -> true
         | _ -> false)
    let strictPins = strictPinVerb && Array.contains "--strict-pins" args
    let args = if strictPins then args |> Array.filter (fun a -> a <> "--strict-pins") else args
    // `--no-cache` is likewise a MODE, not a positional argument: it disables
    // the content-addressed executable cache for the verbs that own a g++
    // invocation. Like `--memcheck`, it travels as a process-level env pin
    // rather than a parameter, because the gate is read in Build.fs -- five
    // test blocks and the REPL reach that compile without passing through any
    // CLI-shaped option record. Stripped from argv so every verb pattern
    // accepts it in any position.
    let noCacheVerb =
        args.Length >= 1 &&
        (match args.[0] with
         | "compile" | "run" | "test" -> true
         | _ -> false)
    if noCacheVerb && Array.contains "--no-cache" args then
        System.Environment.SetEnvironmentVariable("BLADE_EXE_CACHE", "0")
    let args = if noCacheVerb then args |> Array.filter (fun a -> a <> "--no-cache") else args
    match args with
    // User-facing commands.
    // `run <file> [--verbose] [--mpi N] [--memcheck]` -- flags in any order
    // after the file.
    | _ when args.Length >= 2 && args.[0] = "run" ->
        let rest = args.[1..] |> Array.toList
        let mutable verbose = false
        let mutable mpiRanks = None
        let mutable file = None
        let mutable bad = None
        let rec parse toks =
            match toks with
            | [] -> ()
            | "--verbose" :: tl ->
                verbose <- true
                // Build.fs's executable cache reports `[cache] hit/store
                // <hash8>` on stderr under this pin (it has no verbose
                // parameter of its own; see exeCacheVerbose).
                System.Environment.SetEnvironmentVariable("BLADE_EXE_CACHE_VERBOSE", "1")
                parse tl
            | "--cuda" :: tl ->
                // Flip device-kernel emission for `where cuda` licences --
                // the same gate the CUDA test harness sets (setCudaEmitMode).
                // Downstream is untouched: emitted __global__ kernels make the
                // backend inference pick .cu + the nvcc split-compile path.
                // Off by default so a licence alone never changes the compile
                // toolchain out from under a plain `blade run`.
                CodeGen.setCudaEmitMode true
                parse tl
            | "--memcheck" :: tl ->
                // A process-level pin rather than a parameter: CodeGen (the
                // blade_memcheck.hpp include), Build (the Debug+ASan compile
                // profile and the longer run timeout) all read BLADE_MEMCHECK
                // at their own sites, and exporting the variable directly is
                // the equivalent harness spelling.
                System.Environment.SetEnvironmentVariable("BLADE_MEMCHECK", "1")
                parse tl
            | "--mpi" :: n :: tl ->
                (match System.Int32.TryParse n with
                 | true, v when v > 0 -> mpiRanks <- Some v; parse tl
                 | _ -> bad <- Some $"--mpi expects a positive rank count, got '{n}'")
            | ["--mpi"] -> bad <- Some "--mpi requires a rank count (e.g. run prog.blade --mpi 4)"
            | f :: tl when file.IsNone && not (f.StartsWith "--") -> file <- Some f; parse tl
            | f :: _ -> bad <- Some $"unexpected argument '{f}'"
        parse rest
        match bad, file with
        | Some msg, _ -> eprintfn "Error: %s" msg; 1
        | None, None -> usageFailure "run needs a source file (e.g. run prog.blade)"
        | None, Some f -> runFile f verbose mpiRanks strictPins

    | [| "compile"; file |] ->
        match compileToExe file None false strictPins with
        | Ok path -> printfn "%s" path; 0
        | Error e -> reportFailure e
    | [| "compile"; file; "-o"; output |] ->
        match compileToExe file (Some output) false strictPins with
        | Ok path -> printfn "%s" path; 0
        | Error e -> reportFailure e

    | [| "emit"; file |] -> emitFile file None false strictPins
    | [| "emit"; file; "-o"; output |] -> emitFile file (Some output) false strictPins
    | [| "emit"; file; "--verbose" |] -> emitFile file None true strictPins
    | [| "emit"; file; "-o"; output; "--verbose" |] -> emitFile file (Some output) true strictPins

    | [| "check"; file |] -> checkFile file strictPins

    // The optimization decision record (Blade.Effects.Decisions).
    | [| "plan"; file |] -> planFile file false
    | [| "plan"; file; "--json" |] | [| "plan"; "--json"; file |] -> planFile file true

    // Native-toolchain health report (docs/plans/plan-toolchain-packaging.md).
    | [| "doctor" |] -> Blade.Doctor.runDoctor false
    | [| "doctor"; "--json" |] -> Blade.Doctor.runDoctor true

    // Environment bootstrap: verify-then-persist configuration modes.
    | _ when args.Length >= 1 && args.[0] = "setup" ->
        Blade.Setup.runSetup (args.[1..] |> Array.toList)

    | [| "repl" |] -> replLoop ()

    // Editor tooling (JSON on stdout; see Ide.fs).
    | [| "ide"; "check"; "--json"; file |]
    | [| "ide"; "check"; file; "--json" |]
    | [| "ide"; "check"; file |] -> Blade.Ide.ideCheck file

    // The same payload, served: one long-lived process, NDJSON both ways.
    | [| "ide"; "serve" |] -> Blade.IdeServe.serve compilerVersion

    // The language surface as data -- what protocol/surface.json is generated
    // from. Redirect it with a tool that writes LF and no BOM (PowerShell `>`
    // writes both); `blade test surface` pins the committed copy against a
    // live render.
    | [| "ide"; "surface" |] -> printfn "%s" (Blade.Ide.renderSurface compilerVersion); 0

    | _ when args.Length >= 1 && args.[0] = "test" ->
        dispatchTest (args.[1..] |> Array.toList)

    // Backward-compat flags.
    | [||] -> runFullSuite defaultFullSuiteOptions
    | [| "--full" |] -> runFullSuite defaultFullSuiteOptions
    | [| "--help" |] -> printUsage (); 0
    | _ -> usageFailure ($"""unrecognized command: {(String.Join(" ", args))}""")

/// Top-level error boundary: turns any escaping exception into a rendered
/// diagnostic on stderr (exit 1) instead of a raw .NET stack trace. A typed
/// BladeDiagnosticException renders as itself; any other exception becomes a
/// BL9001 internal compiler error (.NET stack shown only under --verbose).
/// Existing eprintfn error paths inside dispatchInner are untouched.
let dispatch (args: string[]) : int =
    let verbose = args |> Array.contains "--verbose"
    try
        dispatchInner args
    with
    | Blade.Diagnostics.BladeDiagnosticException d ->
        let useColor = not System.Console.IsErrorRedirected
        eprintfn "%s" (Blade.Diagnostics.Render.render useColor None d)
        1
    | ex ->
        // An exception is allowed to carry an empty Message (a bare `raise
        // (Exception())`, some interop failures); falling back to the type name
        // keeps the ICE from reading as "internal compiler error: " with
        // nothing after the colon.
        let detail =
            if System.String.IsNullOrWhiteSpace ex.Message then ex.GetType().FullName
            else ex.Message
        let d = Blade.Diagnostics.Codes.ice detail
        let useColor = not System.Console.IsErrorRedirected
        eprintfn "%s" (Blade.Diagnostics.Render.render useColor None d)
        if verbose then eprintfn "%s" (ex.ToString())
        1
