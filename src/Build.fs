// Toolchain probing and C++/CUDA build orchestration for the Blade compiler:
// capability detection, compileCpp/compileCuda/compileCudaSplit, executable
// running, and the backend-requirement resolution shared by the CLI and the
// test harness.
module Blade.Build

open System
open System.IO
open System.Diagnostics
open System.Runtime.InteropServices

type Process = System.Diagnostics.Process
type ProcessStartInfo = System.Diagnostics.ProcessStartInfo

// Backend capability detection + toolchain resolution.
//
// CUDA is a backend *mode*: which test targets the device is decided during
// codegen, so by compile time the backend requirement is *inferred* from
// the generated output (presence of device kernels), not declared per-test.
// Capabilities are advertised once at startup and intersected against each
// test's inferred requirement; an unsatisfiable requirement is SKIPPED with
// a reason, not failed. The host-compiler choice is a per-(platform,
// backend) resolution, never a per-test axis.

// Shared host-compiler optimization flags: ONE value, consumed by
// compileCppWithExtra AND by every test block that shells out to g++ on its
// own (tests/Differential.fs, Benchmarks.fs, OmpTests.fs, AllocTests.fs,
// OrbWreathTests.fs) via `Build.optFlags`, since Build.fs compiles first in
// Blade.fsproj. Excludes `-std=`, a per-site property (orb_wreath_tests.cpp
// needs c++20).
//
// `-march=native` lets GCC use the build machine's full ISA. BLADE_MARCH
// makes that reproducible:
//   unset  -> `native` (default)
//   `off`  -> no -march flag (portable / cross-machine repro)
//   other  -> `-march=<value>` verbatim (e.g. `skylake`, `x86-64-v3`)
//
// `-ffp-contract` defaults to `fast` (FMA on). Contraction fuses a*b+c into
// one rounding, which breaks the BYTE-IDENTITY differential harnesses:
// src/Interp/Numerics.fs is bit-pinned to non-FMA scalar semantics. Those
// harnesses PIN `BLADE_FP_CONTRACT=off` for their own runs (tests/
// InterpDiff.fs, tests/DiffOracle.fs), since byte-identity is a property
// of the differential gates, not of user builds.
//   unset  -> `fast` (default: FMA on)
//   other  -> `-ffp-contract=<value>` verbatim (`fast` | `on` | `off`)
// The `fma(a, b, c)` intrinsic is the explicit, lane-identical form: it is its
// own IR node rendered std::fma / Math.FusedMultiplyAdd / llvm.fma.f64, so it
// fuses under `off` and cannot be unfused under `fast` -- write it where the
// single rounding IS the algorithm (error-free transformations, double-double).
//
// `-fno-math-errno` is unconditional and is a VECTORIZATION flag, not a
// fast-math one. Without it GCC must treat `sqrt` (and fabs/floor/ceil/trunc/
// round) as a call that may write the global `errno`, which is a side effect no
// vector form can reproduce -- so a `sqrt(a)` map over an array does not
// vectorize at all under `-O3 -march=native`. With it the same loop reports
// "loop vectorized using 32 byte vectors". The dropped side effect is
// unobservable here: `src/cpp/` never reads `errno`, and the VALUES are
// untouched (IEEE-754 sqrt is correctly rounded either way), so this is
// bit-exact and safe for the byte-identity differential gates.
//
// These are FUNCTIONS, not module-level values, so a harness may set the
// env var mid-process and have it honored by the next compile -- a
// module-level `let` would freeze the default at first touch.
//
// The CUDA paths below stay at -O2: nvcc translates host flags for its host
// compiler (cl.exe on Windows) and -march does not pass through cleanly there.

/// The `-march=` fragment (leading space included, or "" when disabled).
let private marchFlag () =
    match System.Environment.GetEnvironmentVariable("BLADE_MARCH") with
    | null | "" -> " -march=native"
    | v when v.Trim().ToLowerInvariant() = "off" -> ""
    | v -> $" -march={v.Trim()}"

/// The `-ffp-contract=` fragment (leading space included).
let private fpContractFlag () =
    match System.Environment.GetEnvironmentVariable("BLADE_FP_CONTRACT") with
    | null | "" -> " -ffp-contract=fast"
    | v -> $" -ffp-contract={v.Trim()}"

/// Host-compiler optimization flags shared by every g++ invocation.
/// Currently `-O3 -march=native -ffp-contract=fast -fno-math-errno` by default
/// (see the env vars above; the errno flag is unconditional).
/// Re-evaluated per call so harness env pins take effect.
let optFlags () = "-O3" + marchFlag () + fpContractFlag () + " -fno-math-errno"

// ---------------------------------------------------------------------------
// The BLADE_LLVM lane's gates (docs/plans/plan-llvm-backend.md section 5).
//
// FUNCTIONS, never module-level `let`s, for the reason stated above: a harness
// (or a sweep script) that pins BLADE_LLVM mid-process must have the pin
// honored on the next call.
// ---------------------------------------------------------------------------

/// Whether `blade run` / `compile` / `emit` try the direct LLVM back end first.
/// UNSET IS THE DEFAULT AND MEANS OFF: with the variable absent every lane in
/// the compiler behaves byte-for-byte as it did before the back end existed.
let llvmEnabled () : bool =
    match System.Environment.GetEnvironmentVariable("BLADE_LLVM") with
    | "1" | "on" -> true
    | _ -> false

/// Optimization flags for the clang lane. `-O3` plus the SAME `BLADE_MARCH`
/// mapping the g++ lane uses, so an A/B between the two back ends compares
/// like with like. No `-ffp-contract`: LLVM IR contracts only where the
/// `contract` fast-math flag is present, and EmitLlvm emits none -- the
/// default emission is already byte-identity-shaped.
///
/// `-fno-math-errno` is carried for flag PARITY with `optFlags`, but measure
/// before crediting it: on this lane clang's input is a `.ll`, and the flag
/// only changes what the C FRONT END emits. It cannot retroactively annotate
/// declarations that are already in the IR, and `EmitLlvm.libmUnary` declares
/// `@sqrt` with `nofree nounwind willreturn` -- no `memory(none)` -- so LLVM
/// must still assume the call writes errno. A/B measured on a 1023-cell
/// `sqrt` map: the loop vectorizes with the flag NEITHER on nor off. Closing
/// the gap means emitting the attribute in EmitLlvm, not adding a flag here.
let llvmOptFlags () = "-O3" + marchFlag () + " -fno-math-errno"

type HostPlatform = PWindows | PLinux | PMacOS

/// The environment's toolchain capabilities.
///
/// Every field is backed by its OWN memoized probe, forced on first read and
/// never again in the process. Property syntax at the call sites is unchanged
/// (`caps.HasGpp`), but the cost model is: a consumer pays only for the tools
/// it actually asks about. That matters because the probes are subprocess
/// launches on wildly different budgets (measured: g++ 167 ms, nvcc 138 ms,
/// cl 52 ms, `nvidia-smi -L` 510 ms) while the overwhelmingly common consumer
/// -- the CpuOnly / RequiresMpi arms of `resolveCompile`, i.e. every plain
/// `blade compile` / `blade run` -- reads `HasGpp` and nothing else. Probing
/// all four eagerly cost ~700 ms on every such invocation.
///
/// A "report the environment" consumer (the harness's end-of-run banner) does
/// legitimately read all four, and pays for all four, once.
[<Sealed>]
type Capabilities (platform: HostPlatform,
                   gpp: Lazy<bool>,
                   nvcc: Lazy<bool>,
                   cl: Lazy<bool>,
                   gpu: Lazy<bool>) =
    member _.Platform = platform
    member _.HasGpp   = gpp.Value
    member _.HasNvcc  = nvcc.Value
    member _.HasCl    = cl.Value      // cl.exe on PATH (the host compiler nvcc drives on Windows)
    member _.HasGpu   = gpu.Value     // a runnable CUDA device is present

/// Backend requirement inferred from generated source. `RequiresCuda` when
/// codegen emitted at least one device kernel; `RequiresMpi` when the program
/// includes <mpi.h> (needs the per-OS MPI link flag and mpiexec at run); `CpuOnly` otherwise.
type BackendReq = CpuOnly | RequiresCuda | RequiresMpi

/// Resolution of (capabilities, requirement) into a concrete compile action.
type CompilePlan =
    | UseGpp
    | UseNvcc                 // nvcc drives host compiler: cl.exe (Windows) / g++ (Linux)
    | SkipCompile of string   // human-readable reason

/// Probe whether a tool responds to a version/help query on PATH. Public:
/// `blade doctor` probes setup-adjacent tools (make, gfortran, git, coqc)
/// through the same helper rather than growing a twin.
let probeTool (exe: string) (args: string) : bool =
    try
        let psi = ProcessStartInfo(exe, args)
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.UseShellExecute <- false
        psi.CreateNoWindow <- true
        use proc = Process.Start(psi)
        // Drain to avoid pipe deadlock; we only care that it launched + exited.
        proc.StandardOutput.ReadToEnd() |> ignore
        proc.StandardError.ReadToEnd() |> ignore
        proc.WaitForExit(10000) |> ignore
        proc.ExitCode = 0
    with _ -> false

/// Marker-based tool probe: success = the tool launched and its combined
/// output contains `marker`, for tools whose exit codes are not
/// trustworthy as a presence signal (MS-MPI's mpiexec exits nonzero from a bare help query).
let private probeToolLoose (exe: string) (args: string) (marker: string) : bool =
    try
        let psi = ProcessStartInfo(exe, args)
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.UseShellExecute <- false
        psi.CreateNoWindow <- true
        use proc = Process.Start(psi)
        let out = proc.StandardOutput.ReadToEnd()
        let err = proc.StandardError.ReadToEnd()
        proc.WaitForExit(10000) |> ignore
        (out + err).IndexOf(marker, StringComparison.OrdinalIgnoreCase) >= 0
    with _ -> false

/// Probe for a runnable CUDA device: `nvidia-smi -L` lists devices and exits
/// 0 with a non-empty list when at least one GPU is present -- a proxy for a
/// real `cudaGetDeviceCount` probe that avoids compiling one.
let private probeGpu () : bool =
    try
        let psi = ProcessStartInfo("nvidia-smi", "-L")
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.UseShellExecute <- false
        psi.CreateNoWindow <- true
        use proc = Process.Start(psi)
        let out = proc.StandardOutput.ReadToEnd()
        proc.StandardError.ReadToEnd() |> ignore
        proc.WaitForExit(10000) |> ignore
        proc.ExitCode = 0 && out.Contains("GPU")
    with _ -> false

/// The host platform. A pure remap of `Platforms.os` -- the ONE detection
/// site -- so no subprocess is involved and it is computed eagerly wherever
/// a Capabilities is built.
let private hostPlatform () =
    match Platforms.os with
    | Platforms.Windows -> PWindows
    | Platforms.MacOS -> PMacOS
    | Platforms.Linux -> PLinux

// One memoized probe per tool, at module level so the memo outlives any
// individual Capabilities value (repeat `detectCapabilities ()` calls share
// them). `lazy` in F# is LazyThreadSafetyMode.ExecutionAndPublication, which
// is what the parallel test harness needs: at most one subprocess per tool per
// process, no matter how many threads ask at once.
//
// These stay probes-behind-lazies rather than cached booleans read from the
// environment: nothing here consults an env var, so the "env gates are
// functions" rule (marchFlag/fpContractFlag above, LinAlgPatterns' BLAS gate)
// is untouched -- a harness that pins BLADE_* mid-process still gets the pin
// honored, because those gates were never part of this record.
let private gppProbe  : Lazy<bool> = lazy (probeTool "g++" "--version")
let private nvccProbe : Lazy<bool> = lazy (probeTool "nvcc" "--version")
/// PATH presence of clang, for the BLADE_LLVM lane. Memoized alongside the
/// other tool probes and exempt from the "gates are functions" rule for the
/// same reason they are: it consults no environment variable, only whether a
/// tool answers.
let private clangPathProbe : Lazy<bool> = lazy (probeTool "clang" "--version")
let private clProbe   : Lazy<bool> = lazy (hostPlatform () = PWindows && probeTool "cl" "/?")
let private gpuProbe  : Lazy<bool> = lazy (probeGpu ())

/// Build the capability view. Free to call: it wires up the shared per-tool
/// lazies and runs no probe by itself.
let detectCapabilities () : Capabilities =
    Capabilities(hostPlatform (), gppProbe, nvccProbe, clProbe, gpuProbe)

/// Capabilities are environment-global; one shared view for every consumer.
let capabilities = lazy (detectCapabilities ())

/// Resolve the clang that compiles the BLADE_LLVM lane's `.ll`, in the
/// documented order: the BLADE_LLVM_CLANG override (used VERBATIM, so a bad
/// value fails loudly instead of being silently replaced), then `clang` on
/// PATH, then the MSYS2 clang64 root the memcheck lane already depends on.
/// A function, not a value: the override is an environment variable.
let resolveClang () : string option =
    match System.Environment.GetEnvironmentVariable("BLADE_LLVM_CLANG") with
    | null | "" ->
        if clangPathProbe.Value then Some "clang"
        else
            let msys2 = @"C:\msys64\clang64\bin\clang.exe"
            if File.Exists msys2 then Some msys2 else None
    | over -> Some (over.Trim())

/// Whether g++ is actually present and runnable on PATH. Delegates to the
/// same `probeTool "g++" "--version"` probe that resolveCompile/DiffOracle/
/// InterpDiff already consult (a hardcoded `true` would report a box
/// without g++ as a test FAILURE instead of a skip).
let checkGppAvailable () = capabilities.Value.HasGpp

/// Infer the backend requirement from generated source: CUDA codegen emits
/// `__global__`-qualified kernels, CPU codegen never does, so the inference
/// flips automatically once device kernels appear in the output.
let inferBackendReq (generatedSource: string) : BackendReq =
    if generatedSource.Contains("__global__") then RequiresCuda
    elif generatedSource.Contains("#include <mpi.h>") then RequiresMpi
    else CpuOnly

/// Resolve (capabilities, requirement) into a compile action. A test never
/// picks a compiler; it produces a BackendReq and this picks the toolchain.
/// MPI compiles with plain g++ (compileCpp appends the per-OS MPI link flag
/// when it sees the mpi.h include); a missing MPI import lib fails the g++
/// link loudly.
let resolveCompile (caps: Capabilities) (req: BackendReq) : CompilePlan =
    match req, caps.Platform with
    | CpuOnly, _ when not caps.HasGpp           -> SkipCompile "requires g++, not found"
    | CpuOnly, _                                -> UseGpp
    | RequiresMpi, _ when not caps.HasGpp       -> SkipCompile "requires g++, not found"
    | RequiresMpi, _                            -> UseGpp
    | RequiresCuda, _ when not caps.HasNvcc     -> SkipCompile "requires CUDA, nvcc not found"
    | RequiresCuda, PMacOS                      -> SkipCompile "CUDA unsupported on macOS"
    | RequiresCuda, PWindows when not caps.HasCl -> SkipCompile "requires CUDA, cl.exe not found (nvcc host compiler)"
    | RequiresCuda, _                           -> UseNvcc

/// Whether a Result error string denotes a skip (no-toolchain, no-GPU, etc.)
/// rather than a genuine failure. Skips never count against the pass total.
let isSkipError (e: string) =
    e = "Skipped" || e.StartsWith("Skipped:")

/// Run a subprocess, capturing combined output. Shared by the split-compile
/// steps and the device-shim build (`buildCublasDevice`); Ok () on exit 0, else Error with the captured output.
let runProc (exe: string) (args: string) (timeoutMs: int) : Result<unit, string> =
    try
        let psi = ProcessStartInfo(exe, args)
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.UseShellExecute <- false
        psi.CreateNoWindow <- true
        use proc = Process.Start(psi)
        let outT = Blade.Runtime.readToEndOffPool proc.StandardOutput
        let errT = Blade.Runtime.readToEndOffPool proc.StandardError
        if not (proc.WaitForExit(timeoutMs)) then
            (try proc.Kill() with _ -> ())
            Error $"{exe} timed out"
        else
            let combined =
                [ if not (String.IsNullOrWhiteSpace outT.Result) then yield outT.Result
                  if not (String.IsNullOrWhiteSpace errT.Result) then yield errT.Result ]
                |> String.concat "\n"
            if proc.ExitCode = 0 then Ok ()
            else Error $"{exe} failed (exit {proc.ExitCode}):\n{combined}\nCommand: {exe} {args}"
    with ex -> Error $"{exe} exception: {ex.Message}"

/// The include line a cuBLAS-dispatching program carries, analogous to the
/// `blade_linalg.hpp` / `blade_lapack.hpp` sniffs below: codegen writes it
/// exactly when `LinAlgPatterns.resolveNodeRoute` picked the device backend.
let private cublasShimInclude = "#include \"blade_linalg_cuda.hpp\""

/// Builds the DEVICE half of a cuBLAS-dispatching program and returns the
/// link input the host compile needs. A separate translation unit is
/// required because g++ (mingw-w64 on Windows) cannot include
/// `<cuda_runtime.h>` / `<cublas_v2.h>`, so the shim's definitions are
/// compiled by nvcc and reached across an unmangled `extern "C"` boundary.
/// On Windows it is a shared library, not an object: nvcc drives cl.exe,
/// so a device object would carry the MSVC ABI against the host's MinGW
/// ABI -- linking them directly is the fragility `compileCudaSplit` warns
/// about. Instead the nvcc half is a self-contained DLL with dllexport'd
/// `extern "C"` entry points that MinGW links via the export table, so the
/// ABI boundary is the C-ABI call alone.
///
/// The `.cu` is generated here (just an `#include` of the deployed shim
/// header) since it is build plumbing with no per-program content. A
/// missing toolchain is a hard error, not a skip: codegen already emitted
/// `blade_cuda_*` calls under an explicitly-set `BLADE_CUBLAS`, so failing
/// loudly here mirrors `blade_linalg.hpp`'s `#error` guarantee.
let buildCublasDevice (cppFullPath: string) : Result<string, string> =
    let caps = capabilities.Value
    if not caps.HasNvcc then
        Error "cuBLAS dispatch was emitted (BLADE_CUBLAS is on) but nvcc was not found on PATH; \
               the device half of blade_linalg_cuda.hpp cannot be built"
    elif caps.Platform = PWindows && not caps.HasCl then
        Error "cuBLAS dispatch was emitted (BLADE_CUBLAS is on) but cl.exe was not found on PATH; \
               nvcc needs it as its host compiler on Windows (run from a VS x64 Native Tools prompt)"
    elif caps.Platform = PMacOS then
        Error "cuBLAS dispatch was emitted (BLADE_CUBLAS is on) but CUDA is unsupported on macOS"
    else
        let onWindows = caps.Platform = PWindows
        let srcDir = Path.GetDirectoryName(cppFullPath)
        let stem = Path.GetFileNameWithoutExtension(cppFullPath)
        let cuFile = Path.Combine(srcDir, stem + "_cublas.cu")
        let libExt = if onWindows then ".dll" else ".so"
        let libFile = Path.Combine(srcDir, stem + "_cublas" + libExt)
        let cuText =
            String.concat "\n"
                [ "// Generated by Blade.Build.buildCublasDevice -- the DEVICE translation unit"
                  "// for this program's cuBLAS dispatch. nvcc defines __CUDACC__, so the shim"
                  "// header below expands to its definitions rather than its host prototypes."
                  "#include \"blade_linalg_cuda.hpp\""
                  "" ]
        // A failed write is reported, not swallowed: nvcc would otherwise
        // compile whatever stale `.cu` happened to be there.
        match (try File.WriteAllText(cuFile, cuText); None with ex -> Some ex.Message) with
        | Some why -> Error $"could not write the cuBLAS device translation unit {cuFile}: {why}"
        | None ->
        // /Zc:preprocessor: CCCL headers refuse MSVC's traditional
        // preprocessor (same rule as compileCudaSplit / compileCudaMpiHybrid).
        let sharedFlags =
            if onWindows then "-shared -Xcompiler /Zc:preprocessor"
            else "-shared -Xcompiler -fPIC"
        let args =
            $"-std=c++17 -O2 {sharedFlags} -o \"{libFile}\" \"{cuFile}\" -lcublas"
        match runProc "nvcc" args 300000 with
        | Error e -> Error e
        | Ok () -> Ok libFile

/// Best-effort copy of a runtime DLL to the exe's directory, so a memcheck
/// build keeps running outside the shell environment that produced it.
/// `searchDirs` (the compiler's own bin directory) are probed before PATH.
/// Silence on failure is deliberate: the exe still runs fine in any shell
/// whose PATH carries the DLL, so a copy problem must not fail the compile.
let private copyRuntimeDllBesideExe (searchDirs: string list) (exeFullPath: string) (dllName: string) : unit =
    try
        let exeDir = Path.GetDirectoryName(exeFullPath)
        let target = Path.Combine(exeDir, dllName)
        if not (File.Exists target) then
            let pathDirs =
                match Environment.GetEnvironmentVariable "PATH" with
                | null -> []
                | p -> p.Split(Path.PathSeparator) |> Array.toList
            searchDirs @ pathDirs
            |> List.tryPick (fun d ->
                try
                    let c = Path.Combine(d.Trim(), dllName)
                    if File.Exists c then Some c else None
                with _ -> None)
            |> Option.iter (fun src -> File.Copy(src, target, true))
    with _ -> ()

/// Memcheck (BLADE_MEMCHECK=1) compile: a Debug+AddressSanitizer build of the
/// generated C++, pairing the blade_memcheck.hpp instrumentation codegen
/// included with a runtime that actually feeds it allocation events.
///
/// Windows drives cl.exe, NOT g++: the MSYS2 ucrt64 toolchain ships no
/// libasan (`-fsanitize=address` dies at link), while MSVC's ASan is present
/// and its /openmp:llvm front end accepts the `collapse` clauses codegen
/// emits (measured working together with /fsanitize=address, 2026-08-09).
/// cl.exe needs a vcvars64 environment; INCLUDE unset is a hard, actionable
/// error rather than a silent fallback to g++, which would produce a program
/// whose report line says asan=0. /Od /Zi keeps this an honest Debug build;
/// the ASan runtime is ALWAYS a DLL since VS 17.7, so the two runtime DLLs
/// are copied beside the exe afterwards. The .obj (and its /Fd sidecar) are
/// deleted on success -- they'd otherwise litter the .blade's directory --
/// but the linker's .pdb next to the exe is KEPT: ASan symbolizes error
/// stacks from it at run time.
///
/// Deliberately unsupported (clean error, so a census records the skip
/// instead of chasing a broken link): MPI programs, netcdf provider
/// programs, and extra link inputs (nvcc-built device DLLs) -- each needs
/// its own MSVC link recipe that no current memcheck consumer exercises.
///
/// `srcText` is the generated source the caller just wrote to `cppFile`, when
/// it still has it in memory; `None` falls back to reading the file back.
let compileCppMemcheck (srcText: string option) (extraLinkInputs: string list) (cppFile: string) (outputDir: string) : Result<string, string> =
    try
        let onWindows = Platforms.os = Platforms.Windows
        let exeExt = Platforms.exeExtension
        let cppFullPath = Path.GetFullPath(cppFile)
        let exeFullPath = Path.GetFullPath(Path.ChangeExtension(cppFile, exeExt))
        let source =
            match srcText with
            | Some t -> t
            | None -> (try File.ReadAllText cppFullPath with _ -> "")
        if not (List.isEmpty extraLinkInputs) then
            Error "Skipped: memcheck does not support extra link inputs (device DLLs)"
        elif source.Contains "#include <mpi.h>" then
            Error "Skipped: memcheck does not support MPI programs"
        elif source.Contains "#include <netcdf.h>" then
            Error "Skipped: memcheck does not support netcdf provider programs"
        elif onWindows then
            // Preferred Windows lane: MSYS2 clang64 clang++. MSVC's front end
            // dies with C1061 ("blocks nested too deeply") on the deep IIFE
            // chains physics-scale programs emit (~300 nested lambdas at only
            // 72 lexical brace levels, measured 2026-08-09) and no flag
            // raises that limit; clang parses the same file given
            // -fbracket-depth=1024. BLADE_MEMCHECK_CXX overrides the probe
            // for a non-default clang location.
            let clangxx =
                let overridden = Environment.GetEnvironmentVariable "BLADE_MEMCHECK_CXX"
                [ if not (String.IsNullOrEmpty overridden) then yield overridden
                  yield @"C:\msys64\clang64\bin\clang++.exe" ]
                |> List.tryFind File.Exists
            match clangxx with
            | Some cxx ->
                // No -Werror=float-conversion/narrowing here, unlike the g++
                // lane: clang's float-conversion net is wider than gcc's, and
                // a memcheck build is a measurement run, not the enforcement
                // gate the release compile already provides.
                let args =
                    $"-std=c++17 -O0 -g -fopenmp -fsanitize=address -fbracket-depth=1024 -Wno-c++20-extensions -o \"{exeFullPath}\" \"{cppFullPath}\""
                match runProc cxx args 300000 with
                | Error e -> Error e
                | Ok () ->
                    // The clang64 build links its runtimes dynamically; all
                    // four live in the compiler's own bin directory.
                    let cxxDir = Path.GetDirectoryName cxx
                    for dll in [ "libclang_rt.asan_dynamic-x86_64.dll"; "libc++.dll"; "libomp.dll"; "libunwind.dll" ] do
                        copyRuntimeDllBesideExe [cxxDir] exeFullPath dll
                    Ok exeFullPath
            | None ->
            // Fallback: MSVC cl.exe (measured working for shallow programs;
            // /openmp:llvm accepts codegen's collapse clauses alongside
            // /fsanitize=address). Requires a vcvars64 environment.
            if not capabilities.Value.HasCl then
                Error "memcheck requires MSYS2 clang64 (pacman -S mingw-w64-clang-x86_64-clang mingw-w64-clang-x86_64-compiler-rt mingw-w64-clang-x86_64-llvm-openmp) or cl.exe on PATH (vcvars64 / VS x64 Native Tools environment)"
            elif String.IsNullOrEmpty(Environment.GetEnvironmentVariable "INCLUDE") then
                Error "memcheck found cl.exe but INCLUDE is unset -- run from a vcvars64 / VS x64 Native Tools environment (or install MSYS2 clang64)"
            else
                let objPath = Path.ChangeExtension(cppFullPath, ".obj")
                let fdPath = Path.ChangeExtension(cppFullPath, "_obj.pdb")
                let args =
                    $"/nologo /fsanitize=address /Zi /Od /MT /std:c++17 /EHsc /openmp:llvm /Fo\"{objPath}\" /Fd\"{fdPath}\" /Fe\"{exeFullPath}\" \"{cppFullPath}\""
                match runProc "cl" args 300000 with
                | Error e -> Error e
                | Ok () ->
                    for leftover in [objPath; fdPath] do
                        try File.Delete leftover with _ -> ()
                    // clang_rt DLL: required (ASan is dynamic-only since VS
                    // 17.7). libomp DLL: only used when a parallel region
                    // actually runs, same best-effort copy either way.
                    copyRuntimeDllBesideExe [] exeFullPath "clang_rt.asan_dynamic-x86_64.dll"
                    copyRuntimeDllBesideExe [] exeFullPath "libomp140.x86_64.dll"
                    Ok exeFullPath
        else
            // Linux/macOS: g++/clang++ carry ASan natively; -O0 -g mirrors
            // the /Od /Zi profile. LeakSanitizer (where the platform has it)
            // comes for free on top of the BLADE-MEMCHECK report line.
            let args =
                $"-std=c++17 -O0 -g -fopenmp -fsanitize=address -Werror=float-conversion -Werror=narrowing -o \"{exeFullPath}\" \"{cppFullPath}\""
            match runProc "g++" args 300000 with
            | Error e -> Error e
            | Ok () -> Ok exeFullPath
    with ex ->
        Error $"Memcheck compilation exception: {ex.Message}\n{ex.StackTrace}"

// ---------------------------------------------------------------------------
// Content-addressed executable cache (docs/plan-compile-speed.md Stage 4.1)
//
// 89% of a full `blade test` is g++, and a suite re-run compiles a translation
// unit byte-identical to the one it compiled last time. The cache turns that
// re-compile into a file copy: key = SHA256 over everything g++ reads or is
// told (compiler identity, command line, the .cpp, the 13 deployed runtime
// headers, the identity of every explicitly-linked DLL), value = the produced
// .exe under %LOCALAPPDATA%\Blade\exe-cache.
//
// The key errs toward OVER-invalidation: every emission-relevant env gate
// (BLADE_MARCH / BLADE_FP_CONTRACT / BLADE_BLAS+OPENBLAS_DIR / BLADE_CUBLAS /
// NETCDF_DIR) reaches the key through the flags or the source text it already
// changes, so no gate needs its own hash term -- but a gate that changed
// NEITHER could not have changed the output either.
// ---------------------------------------------------------------------------

/// Where the cache lives, or `None` when it is off. Read PER CALL like every
/// other env gate in this file (a harness may pin it mid-process):
///   unset | `1` | `on` | `true`  -> %LOCALAPPDATA%\Blade\exe-cache
///   `0` | `off` | `false`        -> disabled
///   an ABSOLUTE path             -> that directory
///   anything else                -> disabled (an unreadable setting must not
///                                   silently serve stale binaries)
let private exeCacheDir () : string option =
    let defaultDir () =
        let root = Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData)
        if String.IsNullOrEmpty root then None
        else Some (Path.Combine(root, "Blade", "exe-cache"))
    match Environment.GetEnvironmentVariable "BLADE_EXE_CACHE" with
    | null | "" -> defaultDir ()
    | v ->
        match v.Trim() with
        | "" -> defaultDir ()
        | t when t = "1" || t.ToLowerInvariant() = "on" || t.ToLowerInvariant() = "true" -> defaultDir ()
        | t when t = "0" || t.ToLowerInvariant() = "off" || t.ToLowerInvariant() = "false" -> None
        | t when Path.IsPathRooted t -> Some t
        | _ -> None

/// `[cache] hit/store <hash8>` tracing on stderr. `compileCppWithExtraSource`
/// takes no verbose parameter (it is reached from the CLI, the REPL and five
/// test blocks), so the flag travels as a process-level env pin -- the same
/// spelling `--memcheck` uses for BLADE_MEMCHECK. `blade run --verbose` sets it
/// during argument parsing (Cli.fs).
let private exeCacheVerbose () =
    match Environment.GetEnvironmentVariable "BLADE_EXE_CACHE_VERBOSE" with
    | null | "" | "0" -> false
    | _ -> true

/// The compiler's identity: resolved g++ path + the first line of
/// `g++ --version`. Memoized for the process (one subprocess, ~50-170 ms, and
/// only on the first compile of a run) -- a g++ upgrade mid-process is not a
/// case worth a probe per compile. `lazy` is ExecutionAndPublication, so the
/// parallel harness launches it at most once.
let private gppIdentity : Lazy<string> =
    lazy (
        let resolved =
            let exeName = if Platforms.os = Platforms.Windows then "g++.exe" else "g++"
            match Environment.GetEnvironmentVariable "PATH" with
            | null -> None
            | p ->
                p.Split(Path.PathSeparator)
                |> Array.tryPick (fun d ->
                    try
                        if String.IsNullOrWhiteSpace d then None
                        else
                            let c = Path.Combine(d.Trim(), exeName)
                            if File.Exists c then Some c else None
                    with _ -> None)
        let version =
            try
                let psi = ProcessStartInfo("g++", "--version")
                psi.RedirectStandardOutput <- true
                psi.RedirectStandardError <- true
                psi.UseShellExecute <- false
                psi.CreateNoWindow <- true
                use proc = Process.Start(psi)
                let out = Blade.Runtime.readToEndOffPool proc.StandardOutput
                Blade.Runtime.readToEndOffPool proc.StandardError |> ignore
                proc.WaitForExit(10000) |> ignore
                let text = out.Result
                match text.Split('\n') |> Array.tryHead with
                | Some l -> l.Trim()
                | None -> ""
            with _ -> ""
        $"""{(defaultArg resolved "g++")}|{version}""")

/// What `-march=native` ACTUALLY SELECTED on this machine, hashed.
///
/// `-march=native` is CPU-dependent codegen behind CPU-INDEPENDENT TEXT. The
/// flag reads the same everywhere, so two machines with different CPUs hash to
/// the same key for binaries that are not interchangeable, and a cache shared
/// between them hands one machine a binary built for the other's instruction
/// set. That is not hypothetical: a CI cache shared across heterogeneous
/// runners did exactly this, and every compile-and-run test in the lane
/// "compiled" -- a cache hit is a file copy -- and then died with
/// STATUS_ILLEGAL_INSTRUCTION (0xC000001D), including one with no provider or
/// external library in it at all.
///
/// Asking the compiler is the authoritative answer: `-Q --help=target` prints
/// the target options `native` resolved to, so a CPU difference that changes
/// codegen changes this string and one that does not, does not. Memoized like
/// `gppIdentity` (one subprocess per run, on the first compile), and empty on
/// any failure, which returns the key to exactly what it was before.
let private nativeTargetIdentity : Lazy<string> =
    lazy (
        try
            let psi = ProcessStartInfo("g++", "-march=native -Q --help=target")
            psi.RedirectStandardOutput <- true
            psi.RedirectStandardError <- true
            psi.UseShellExecute <- false
            psi.CreateNoWindow <- true
            use proc = Process.Start(psi)
            let out = Blade.Runtime.readToEndOffPool proc.StandardOutput
            Blade.Runtime.readToEndOffPool proc.StandardError |> ignore
            proc.WaitForExit(10000) |> ignore
            use sha = System.Security.Cryptography.SHA256.Create()
            sha.ComputeHash(System.Text.Encoding.UTF8.GetBytes out.Result)
            |> Array.map _.ToString("x2")
            |> String.concat ""
        with _ -> "")

/// The runtime headers' contribution to the key, computed once per process:
/// all 13 shipped header texts (~264 KB), name-tagged. They are static files
/// beside the binary and already memoized by CodeGen, so hashing them costs
/// one SHA pass on the first compile and nothing afterwards.
///
/// This hashes the SHIPPED headers, while g++ reads the copies deployed beside
/// the .cpp -- `deployRuntimeHeaders` runs before every compile and rewrites
/// any deployed file whose content differs, so at g++ time the two agree
/// (that is exactly the hand-edit workflow its doc comment describes).
let private runtimeHeaderDigest : Lazy<string> =
    lazy (
        try
            use sha = System.Security.Cryptography.SHA256.Create()
            let sb = System.Text.StringBuilder()
            for name in CodeGen.runtimeHeaderNames do
                sb.Append(name).Append(' ').Append(CodeGen.runtimeHeaderText name).Append(' ') |> ignore
            sha.ComputeHash(System.Text.Encoding.UTF8.GetBytes(sb.ToString()))
            |> Array.map _.ToString("x2")
            |> String.concat ""
        with _ -> "")

/// Size + mtime of every DLL named outright on the link line (netcdf.dll,
/// libopenblas.dll). Their PATH is already in `args`, but their CONTENT is
/// not: a reinstalled OpenBLAS at the same path must invalidate.
let private linkedDllStamp (args: string) : string =
    args.Split('"')
    |> Array.filter _.EndsWith(".dll", StringComparison.OrdinalIgnoreCase)
    |> Array.map (fun p ->
        try
            let fi = FileInfo(p)
            if fi.Exists then $"{p}:{fi.Length}:{fi.LastWriteTimeUtc.Ticks}" else $"{p}:missing"
        with _ -> $"{p}:?")
    |> String.concat ";"

/// The cache key for one g++ invocation. `exeFullPath`/`cppFullPath` are
/// replaced by placeholders: WHERE the translation unit sits does not change
/// what g++ produces from it, and that is what lets the same program compiled
/// in two directories share one entry.
let private exeCacheKey (args: string) (cppText: string) (exeFullPath: string) (cppFullPath: string) : string =
    let normalizedArgs =
        args.Replace(exeFullPath, "<EXE>").Replace(cppFullPath, "<CPP>")
    let material =
        String.concat " "
            [ "blade-exe-cache-v1"
              gppIdentity.Value
              normalizedArgs
              // Only when the flag is `native`, and deliberately: an explicit
              // `-march=x86-64-v3` is portable text `normalizedArgs` already
              // carries, and `BLADE_MARCH=off` selects nothing CPU-specific.
              // So the common non-native configurations keep the keys they had
              // rather than being invalidated for a hazard they do not have.
              (if (marchFlag ()).Contains "native" then nativeTargetIdentity.Value else "")
              runtimeHeaderDigest.Value
              linkedDllStamp args
              cppText ]
    use sha = System.Security.Cryptography.SHA256.Create()
    sha.ComputeHash(System.Text.Encoding.UTF8.GetBytes material)
    |> Array.map _.ToString("x2")
    |> String.concat ""

// Eviction caps. Entries are whole executables (~150 KB-2 MB each), so both a
// count and a byte ceiling are needed; a trip on either prunes oldest-mtime
// first down to 3/4 of the cap, so eviction runs rarely rather than on every
// store past the line.
let private exeCacheMaxEntries = 8192
let private exeCacheMaxBytes = 6L * 1024L * 1024L * 1024L

/// Prune the cache when either ceiling is exceeded. Called only on STORE (a
/// hit does no directory scan). Every delete is race-tolerant: a concurrent
/// process may have removed or be reading the same entry.
let private evictExeCache (dir: string) : unit =
    try
        let entries = DirectoryInfo(dir).GetFiles("*.exe")
        let total = entries |> Array.sumBy _.Length
        if entries.Length > exeCacheMaxEntries || total > exeCacheMaxBytes then
            let targetCount = (exeCacheMaxEntries * 3) / 4
            let targetBytes = (exeCacheMaxBytes / 4L) * 3L
            let oldestFirst = entries |> Array.sortBy _.LastWriteTimeUtc
            let mutable count = entries.Length
            let mutable bytes = total
            for f in oldestFirst do
                if count > targetCount || bytes > targetBytes then
                    let len = f.Length
                    try
                        f.Delete()
                        count <- count - 1
                        bytes <- bytes - len
                    with _ ->
                        // Another process holds or already removed it; it still
                        // stops counting against us on the next scan.
                        count <- count - 1
                        bytes <- bytes - len
    with _ -> ()

// ---------------------------------------------------------------------------
// The tile store of revision reuse (docs/plans/structural/04, 3.1): a local
// on-disk store beside the exe cache, gated by BLADE_TILE_CACHE with the exe
// cache's grammar EXCEPT that unset means OFF (the mechanism is opt-in). The
// generated program locates it at run time (blade_tilecache.hpp's `dir()`);
// the compiler reads the same variable to decide whether to emit tile
// phases at all (CodeGenTiles.tileCacheEnabled) and to evict here.
// ---------------------------------------------------------------------------

let tileCacheDir () : string option =
    let defaultDir () =
        let root = Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData)
        if String.IsNullOrEmpty root then None
        else Some (Path.Combine(root, "Blade", "tile-cache"))
    match Environment.GetEnvironmentVariable "BLADE_TILE_CACHE" with
    | null | "" -> None
    | v ->
        match v.Trim() with
        | "" -> None
        | t when t = "1" || t.ToLowerInvariant() = "on" || t.ToLowerInvariant() = "true" -> defaultDir ()
        | t when t = "0" || t.ToLowerInvariant() = "off" || t.ToLowerInvariant() = "false" -> None
        | t when Path.IsPathRooted t -> Some t
        | _ -> None

/// The exe cache's caps, applied to tile files: oldest-mtime first, down to
/// 3/4 of whichever cap tripped. BOTH caps matter and for the exe cache's
/// reason -- a tile holds one chunk's worth of OUTPUT CELLS, so a coarse
/// grid makes them large (8 tiles of a 4000 x 4000 float64 field are 128 MB,
/// measured) and a count cap alone would let the store reach hundreds of
/// gigabytes before it evicted anything.
let private evictTileCache (dir: string) : unit =
    try
        if Directory.Exists dir then
            let entries = DirectoryInfo(dir).GetFiles("*.tile", SearchOption.AllDirectories)
            let total = entries |> Array.sumBy _.Length
            if entries.Length > exeCacheMaxEntries || total > exeCacheMaxBytes then
                let targetCount = (exeCacheMaxEntries * 3) / 4
                let targetBytes = (exeCacheMaxBytes / 4L) * 3L
                let mutable count = entries.Length
                let mutable bytes = total
                for f in entries |> Array.sortBy _.LastWriteTimeUtc do
                    if count > targetCount || bytes > targetBytes then
                        let len = f.Length
                        (try f.Delete() with _ -> ())
                        count <- count - 1
                        bytes <- bytes - len
    with _ -> ()

/// The toolchain identity a tile-enabled program carries
/// (`-DBLADE_TOOLCHAIN_ID=...`): the exe cache's key terms other than the
/// program text and the DLL stamp -- compiler, flags, what `-march=native`
/// selected, the runtime headers -- so bits compiled by a different
/// toolchain never share a tile file, without hashing at run time.
let private toolchainIdentity (flags: string) : string =
    let material =
        String.concat " "
            [ "blade-toolchain-v1"
              gppIdentity.Value
              flags
              (if (marchFlag ()).Contains "native" then nativeTargetIdentity.Value else "")
              runtimeHeaderDigest.Value ]
    use sha = System.Security.Cryptography.SHA256.Create()
    sha.ComputeHash(System.Text.Encoding.UTF8.GetBytes material)
    |> Array.map _.ToString("x2")
    |> String.concat ""
    |> fun h -> h.Substring(0, 16)

/// Cache lookup. On a hit the entry is copied to `exeFullPath` (the exact file
/// a real compile would have written) and its mtime is bumped so eviction sees
/// it as recently used. Any failure -- a racing evictor deleted it, the copy
/// was denied -- reports a miss and the real compile proceeds.
let private tryExeCacheHit (dir: string) (key: string) (exeFullPath: string) : bool =
    try
        let entry = Path.Combine(dir, key + ".exe")
        if not (File.Exists entry) then false
        else
            File.Copy(entry, exeFullPath, true)
            // File.Copy carries the SOURCE mtime across on Windows, which
            // would date a hit to whenever the entry was first published.
            // Both files are stamped now: the delivered exe so it looks
            // exactly as freshly built as it behaves, the entry so eviction
            // reads mtime as last-USED.
            let now = DateTime.UtcNow
            (try File.SetLastWriteTimeUtc(exeFullPath, now) with _ -> ())
            (try File.SetLastWriteTimeUtc(entry, now) with _ -> ())
            if exeCacheVerbose () then eprintfn "[cache] hit %s" (key.Substring(0, 8))
            true
    with _ -> false

/// Publish a freshly compiled executable. Written to a unique temp name in the
/// cache directory first, then File.Move'd into place -- the move is atomic
/// within the volume, so a concurrent reader never sees a half-copied entry.
/// Losing the race (another process published the same key first) is a no-op:
/// the two files are the same content by construction.
let private storeExeCache (dir: string) (key: string) (exeFullPath: string) : unit =
    try
        let entry = Path.Combine(dir, key + ".exe")
        if not (File.Exists entry) then
            Directory.CreateDirectory dir |> ignore
            let tmp = Path.Combine(dir, $"""{key}.{(Guid.NewGuid().ToString("N"))}.tmp""")
            File.Copy(exeFullPath, tmp, true)
            (try File.Move(tmp, entry)
             with _ -> (try File.Delete tmp with _ -> ()))
            if exeCacheVerbose () then eprintfn "[cache] store %s" (key.Substring(0, 8))
            evictExeCache dir
    with _ -> ()

/// Compile a C++ file with g++. `extraLinkInputs` are appended after the
/// source (linker order) -- e.g. the hybrid mpi+cuda build passes the
/// nvcc-built device DLL here (MinGW links DLL export tables directly).
/// Under BLADE_MEMCHECK=1 the whole invocation is rerouted to the
/// Debug+ASan profile instead (codegen already included the matching
/// blade_memcheck.hpp instrumentation in the same process).
///
/// `srcText`: the generated source, when the caller still holds the string it
/// wrote to `cppFile` moments ago. Every backend decision below (netcdf, mpi,
/// BLAS/LAPACK, cuBLAS device half) is a substring sniff of that same text, so
/// passing it avoids reading the file back off disk. `None` reads it ONCE and
/// reuses that one read for all four sniffs.
let compileCppWithExtraSource (srcText: string option) (extraLinkInputs: string list) (cppFile: string) (outputDir: string) : Result<string, string> =
    if CodeGen.memcheckEnabled () then compileCppMemcheck srcText extraLinkInputs cppFile outputDir else
    try
        let exeExt = Platforms.exeExtension
        let exeFile = Path.ChangeExtension(cppFile, exeExt)
        
        let cppFullPath = Path.GetFullPath(cppFile)
        let exeFullPath = Path.GetFullPath(exeFile)
        
        let ompFlag = "-fopenmp"

        // The one view of the generated source every sniff below shares:
        // handed in by the caller that just wrote it, or read back exactly
        // once. (This used to be three independent File.ReadAllText calls of
        // the same file, one per sniff.)
        let cppText =
            match srcText with
            | Some t -> t
            | None -> (try File.ReadAllText cppFullPath with _ -> "")

        // Backstops the Blade type system: implicit float->integer narrowing
        // in generated C++ must be a hard error. -Wnarrowing alone only
        // catches brace-init, not assignment; -Wconversion is broader but
        // flags legitimate cases (size_t loop counters vs int literals).
        let safetyFlags = "-Werror=float-conversion -Werror=narrowing"

        // Provider programs emit `#include <netcdf.h>`, needing the netcdf
        // header at compile and library at link -- not on g++'s default
        // search path in the common Windows case (MSVC-built netCDF with
        // an MSVC-format import lib). Resolution (NETCDF_DIR comes through
        // Toolchain.get: process env first, blade.toolchain.json second):
        //   - NETCDF_DIR set: add -I<dir>/include, link the shared library
        //     Platforms.findSharedLib locates under the prefix (the direct
        //     DLL/.so path); falls back to -L<dir>/lib -lnetcdf.
        //   - NETCDF_DIR unset: bare -lnetcdf (default package-manager
        //     install: MSYS2 pacman, apt, brew).
        let needsNetcdf = cppText.Contains "#include <netcdf.h>"

        // MPI programs include <mpi.h> and call the MPI C API -- the MPI
        // dev package puts the header/import lib on g++'s default search
        // paths in the supported installs (MSYS2 mingw-w64 `msmpi` on
        // Windows, OpenMPI/MPICH elsewhere), so the bare per-OS link flag
        // suffices (mirrors -lnetcdf above; Platforms.mpiLinkFlag owns the
        // spelling).
        let needsMpi = cppText.Contains "#include <mpi.h>"
        let mpiFlags = if needsMpi then " " + Platforms.mpiLinkFlag else ""
        let netcdfFlags =
            if not needsNetcdf then ""
            else
                (match Toolchain.get "NETCDF_DIR" with
                 | None -> " -lnetcdf"
                 | Some dir ->
                     let incFlag = sprintf " -I\"%s\"" (Path.Combine(dir, "include"))
                     let linkFlag =
                         match Platforms.findSharedLib dir "netcdf" with
                         | Some lib -> $" \"{lib}\""
                         | None -> sprintf " -L\"%s\" -lnetcdf" (Path.Combine(dir, "lib"))
                     incFlag + linkFlag)

        // Linalg-dispatch programs emit `#include "blade_linalg.hpp"` and
        // call blade_linalg::* for gram/matmul/dot/gemv. The header is
        // BLAS-only (`#error`s without the define); codegen consults the
        // SAME gate this line does (`LinAlgPatterns.blasAvailable`), so
        // within one process "include present" and "gate on" cannot
        // disagree -- an emit and a compile with different gates fails
        // loudly at the `#error` rather than silently miscompiling.
        //
        // Gate semantics are the four TIERS of `LinAlgPatterns.resolveBlasTier`
        // (BLADE_BLAS=0 off / BLADE_BLAS_LINK explicit / OPENBLAS_DIR prefix /
        // BLADE_BLAS=1 bare-system); default-off since BLAS may differ in the
        // last ULP and the differentials demand byte-identical output. The
        // tier's EXPANSION into compile/link halves -- defines, include dirs,
        // library inputs, the MKL header-flavor define -- is
        // `LinAlgPatterns.blasBuildFlags`: one gate, one expansion, consumed
        // here.
        let usesLinalgShim = cppText.Contains "#include \"blade_linalg.hpp\""
        let blasGateOn = Blade.LinAlgPatterns.blasAvailable ()
        // LAPACK gets its own sniff arm and define, so a BLAS-only program
        // stays distinguishable from a LAPACK-carrying one (same
        // #error-on-mismatch guarantee as BLAS). On the OpenBLAS tiers its
        // gate rides the BLAS resolution (LAPACKE is bundled); on the
        // explicit tier it requires BLADE_LAPACK_LINK -- see lapackAvailable.
        let usesLapackShim = cppText.Contains "#include \"blade_lapack.hpp\""
        let lapackGateOn = Blade.LinAlgPatterns.lapackAvailable ()
        // Split into a COMPILE half (defines + -I, precedes the source) and
        // a LINK half (library inputs, follows it); DEFINES stay per-header
        // (`wantsBlas`/`wantsLapack` each gate on include-present AND its own flag).
        let wantsBlas = usesLinalgShim && blasGateOn
        let wantsLapack = usesLapackShim && lapackGateOn
        let (blasCompileFlags, blasLinkFlags) =
            if not (wantsBlas || wantsLapack) then ("", "")
            else Blade.LinAlgPatterns.blasBuildFlags wantsBlas wantsLapack

        // The DEVICE half. `blade_linalg_cuda.hpp`'s include line is written
        // by codegen exactly when a node route resolved to `CudaBlas`; the
        // resulting shared library joins `extraLinkInputs`, linked after
        // the source. Handled here rather than at each caller, so every
        // caller gets it without knowing the backend exists.
        let deviceBuild =
            if cppText.Contains cublasShimInclude then
                buildCublasDevice cppFullPath |> Result.map (fun lib -> [lib])
            else Ok []
        match deviceBuild with
        | Error e -> Error $"cuBLAS device build failed:\n{e}"
        | Ok deviceInputs ->

        let extraFlags = (extraLinkInputs @ deviceInputs) |> List.map (fun p -> $" \"{Path.GetFullPath p}\"") |> String.concat ""
        // The run record's build policy and library routes (blade_run_record.hpp
        // stringizes these). On the COMMAND LINE, not in the emitted text, so
        // the .cpp stays environment-free; the executable cache keys on args.
        let rrDefines =
            let tok (frag: string) =
                match frag.IndexOf '=' with
                | -1 -> "off"
                | i -> frag.Substring(i + 1).Trim()
            let march = (let f = marchFlag () in if f = "" then "off" else tok f)
            let fpc = tok (fpContractFlag ())
            let flag (b: bool) = if b then "1" else "0"
            $" -DBLADE_RR_MARCH={march} -DBLADE_RR_FPC={fpc} -DBLADE_RR_REASSOC={flag (Blade.CodeGenState.fpReassocEnabled ())} -DBLADE_RR_BLAS={flag wantsBlas} -DBLADE_RR_LAPACK={flag wantsLapack} -DBLADE_RR_CUBLAS={flag (not (List.isEmpty deviceInputs))}"
        // Revision reuse: a program with a tiled binding (the include line is
        // written by codegen exactly then) carries the toolchain identity its
        // tile files are keyed under; the store is evicted here, never in C++.
        let tileDefines =
            if cppText.Contains "#include \"blade_tilecache.hpp\"" then
                (match tileCacheDir () with Some d -> evictTileCache d | None -> ())
                let tid = toolchainIdentity (optFlags () + " " + ompFlag)
                $" -DBLADE_TOOLCHAIN_ID={tid}"
            else ""
        let args = $"-std=c++17 {optFlags ()}{rrDefines}{tileDefines} {ompFlag} {safetyFlags}{blasCompileFlags} -o \"{exeFullPath}\" \"{cppFullPath}\"{extraFlags}{netcdfFlags}{mpiFlags}{blasLinkFlags}"
        
        // The executable cache (Stage 4.1, above). v1 scope, deliberately
        // narrow -- every excluded lane is one whose inputs are not fully
        // captured by (args, cppText, headers):
        //   - extra link inputs / the cuBLAS device half: the .dll or .so was
        //     built by another toolchain in this same run; its content is not
        //     in the key.
        //   - non-Windows: %LOCALAPPDATA% has no counterpart here and no
        //     consumer runs there yet.
        //   - memcheck: rerouted to compileCppMemcheck long before this point.
        let cacheSlot =
            if not (List.isEmpty extraLinkInputs) || not (List.isEmpty deviceInputs) then None
            elif Platforms.os <> Platforms.Windows then None
            else
                match exeCacheDir () with
                | None -> None
                | Some dir -> Some (dir, exeCacheKey args cppText exeFullPath cppFullPath)

        match cacheSlot with
        | Some (dir, key) when tryExeCacheHit dir key exeFullPath -> Ok exeFullPath
        | _ ->

        let psi = ProcessStartInfo("g++", args)
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.UseShellExecute <- false
        psi.CreateNoWindow <- true

        use proc = Process.Start(psi)
        // Drain both streams on dedicated threads -- never the thread pool (Runtime.readToEndOffPool)
        let stdoutTask = Blade.Runtime.readToEndOffPool proc.StandardOutput
        let stderrTask = Blade.Runtime.readToEndOffPool proc.StandardError
        
        // 300s: spectra-scale generated programs (rank-2 transforms, capped
        // at 65536 cells) can legitimately push g++ this long under -O3.
        if not (proc.WaitForExit(300000)) then
            try proc.Kill() with _ -> ()
            Error "Compilation timed out after 300s"
        else
        
        let stdout = stdoutTask.Result
        let stderr = stderrTask.Result
        
        let allOutput = 
            [if not (String.IsNullOrWhiteSpace stdout) then yield stdout
             if not (String.IsNullOrWhiteSpace stderr) then yield stderr]
            |> String.concat "\n"
        
        if proc.ExitCode = 0 then
            // Publish for the next identical translation unit. Best-effort:
            // a store that fails costs a future recompile, never this result.
            (match cacheSlot with
             | Some (dir, key) -> storeExeCache dir key exeFullPath
             | None -> ())
            Ok exeFullPath
        else
            if String.IsNullOrWhiteSpace allOutput then
                Error $"Compilation failed (exit {proc.ExitCode}) with no output. Command: g++ {args}"
            else
                Error $"Compilation failed (exit {proc.ExitCode}):\n{allOutput}\nCommand: g++ {args}"
    with ex ->
        Error $"Compilation exception: {ex.Message}\n{ex.StackTrace}"

/// `compileCppWithExtraSource` for a caller that does not hold the generated
/// source in memory (it is read back off disk, once).
let compileCppWithExtra (extraLinkInputs: string list) (cppFile: string) (outputDir: string) : Result<string, string> =
    compileCppWithExtraSource None extraLinkInputs cppFile outputDir

/// Compile a C++ file with g++ (no extra link inputs), passing the generated
/// source the caller just wrote so the backend sniffs need no disk read.
let compileCppSource (srcText: string option) (cppFile: string) (outputDir: string) : Result<string, string> =
    compileCppWithExtraSource srcText [] cppFile outputDir

/// Compile a C++ file with g++ (no extra link inputs).
let compileCpp (cppFile: string) (outputDir: string) : Result<string, string> =
    compileCppWithExtraSource None [] cppFile outputDir

/// Compile a CUDA (.cu) file with nvcc. nvcc auto-selects the host compiler
/// (cl.exe on Windows, g++ on Linux). Host-side warning flags are passed
/// through with -Xcompiler. Mirrors compileCpp's subprocess machinery.
let compileCuda (cuFile: string) (outputDir: string) : Result<string, string> =
    try
        let exeExt = Platforms.exeExtension
        let exeFile = Path.ChangeExtension(cuFile, exeExt)
        let cuFullPath = Path.GetFullPath(cuFile)
        let exeFullPath = Path.GetFullPath(exeFile)

        // Host-compiler passthrough for the narrowing safety net: nvcc's own
        // front-end doesn't accept -Werror=float-conversion, so route it via
        // -Xcompiler (cl.exe uses different flag spellings, so Windows drops
        // the g++-specific ones and relies on nvcc/cl defaults).
        let hostWarn =
            if Platforms.os = Platforms.Windows then
                // CCCL (thrust/complex.h) refuses MSVC's traditional
                // preprocessor; the conforming one is safe for all generated
                // CUDA code, so it is passed unconditionally.
                "-Xcompiler /Zc:preprocessor"
            else "-Xcompiler -Werror=float-conversion,-Werror=narrowing"

        let args = $"-std=c++17 -O2 {hostWarn} -o \"{exeFullPath}\" \"{cuFullPath}\""

        let psi = ProcessStartInfo("nvcc", args)
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.UseShellExecute <- false
        psi.CreateNoWindow <- true

        use proc = Process.Start(psi)
        let stdoutTask = Blade.Runtime.readToEndOffPool proc.StandardOutput
        let stderrTask = Blade.Runtime.readToEndOffPool proc.StandardError

        if not (proc.WaitForExit(120000)) then
            try proc.Kill() with _ -> ()
            Error "CUDA compilation timed out after 120s"
        else

        let stdout = stdoutTask.Result
        let stderr = stderrTask.Result
        let allOutput =
            [if not (String.IsNullOrWhiteSpace stdout) then yield stdout
             if not (String.IsNullOrWhiteSpace stderr) then yield stderr]
            |> String.concat "\n"

        if proc.ExitCode = 0 then
            Ok exeFullPath
        else
            if String.IsNullOrWhiteSpace allOutput then
                Error $"CUDA compilation failed (exit {proc.ExitCode}) with no output. Command: nvcc {args}"
            else
                Error $"CUDA compilation failed (exit {proc.ExitCode}):\n{allOutput}"
    with ex ->
        Error $"CUDA compilation exception: {ex.Message}\n{ex.StackTrace}"

/// Compiles a CUDA program split across two files: nvcc compiles the .cu
/// (device kernels) to an object, g++ compiles the .cpp (host program -- no
/// CUDA syntax, only an extern "C" prototype) to an object, then nvcc links
/// both (resolving the CUDA runtime automatically). The extern "C" launch
/// wrapper is the unmangled boundary symbol both compilers agree on.
let compileCudaSplit (cuFile: string) (cppFile: string) (outputDir: string) : Result<string, string> =
    let onWindows = Platforms.os = Platforms.Windows
    let exeExt = Platforms.exeExtension
    let cuFull = Path.GetFullPath(cuFile)
    let cppFull = Path.GetFullPath(cppFile)
    let objExt = Platforms.objExtension
    let cuObj = Path.ChangeExtension(cuFull, ".cu" + objExt)
    let cppObj = Path.ChangeExtension(cppFull, ".cpp" + objExt)
    let exeFull = Path.GetFullPath(Path.Combine(outputDir, Path.GetFileNameWithoutExtension(cppFile) + exeExt))
    if onWindows then
        // Windows: pure MSVC toolchain, nvcc-orchestrated -- nvcc drives
        // cl.exe as the host compiler for both halves, then links, keeping
        // a single C++ ABI (no cross-ABI link fragility). Requires cl.exe
        // on PATH. No OpenMP here: the rank-1 cuda host half has no
        // parallel loop. /Zc:preprocessor (CCCL refuses MSVC's traditional
        // preprocessor) applies to the .cu compile only.
        let nvccCu  = $"-std=c++17 -O2 -Xcompiler /Zc:preprocessor -c -o \"{cuObj}\" \"{cuFull}\""
        let nvccCpp = $"-std=c++17 -O2 -c -o \"{cppObj}\" \"{cppFull}\""
        let nvccLink = $"-std=c++17 -O2 -o \"{exeFull}\" \"{cuObj}\" \"{cppObj}\""
        match runProc "nvcc" nvccCu 120000 with
        | Error e -> Error e
        | Ok () ->
            match runProc "nvcc" nvccCpp 120000 with
            | Error e -> Error e
            | Ok () ->
                match runProc "nvcc" nvccLink 120000 with
                | Error e -> Error e
                | Ok () -> Ok exeFull
    else
        // Linux: nvcc compiles the .cu (host code via g++), g++ compiles the
        // .cpp; both share the g++ ABI, so the split + link is safe.
        let nvccCu = $"-std=c++17 -O2 -c -o \"{cuObj}\" \"{cuFull}\""
        let gppCpp = $"-std=c++17 -O2 -fopenmp -Werror=float-conversion -Werror=narrowing -c -o \"{cppObj}\" \"{cppFull}\""
        let nvccLink = $"-std=c++17 -O2 -Xcompiler -fopenmp -o \"{exeFull}\" \"{cuObj}\" \"{cppObj}\""
        match runProc "nvcc" nvccCu 120000 with
        | Error e -> Error e
        | Ok () ->
            match runProc "g++" gppCpp 60000 with
            | Error e -> Error e
            | Ok () ->
                match runProc "nvcc" nvccLink 120000 with
                | Error e -> Error e
                | Ok () -> Ok exeFull

/// Hybrid mpi+cuda build: the .cu becomes a self-contained MSVC DLL (nvcc
/// -shared drives cl.exe; the launch wrappers are dllexport'd extern "C"),
/// and the host .cpp takes the g++ path (-fopenmp, -lmsmpi) linking the DLL
/// directly (MinGW reads DLL export tables, as for netcdf.dll) so no
/// MS-MPI SDK import lib or cross-ABI object link is needed.
let compileCudaMpiHybrid (cuFile: string) (cppFile: string) (outputDir: string) : Result<string, string> =
    let caps = capabilities.Value
    if not caps.HasNvcc then Error "Skipped: requires CUDA, nvcc not found"
    elif caps.Platform = PWindows && not caps.HasCl then Error "Skipped: requires CUDA, cl.exe not found (nvcc host compiler)"
    elif not caps.HasGpp then Error "Skipped: requires g++, not found"
    else
        let cuFull = Path.GetFullPath cuFile
        let dllExt = if caps.Platform = PWindows then ".dll" else ".so"
        let dllFull = Path.Combine(Path.GetFullPath outputDir, Path.GetFileNameWithoutExtension cuFile + "_cuda" + dllExt)
        // /Zc:preprocessor on Windows: CCCL (thrust/complex.h) refuses MSVC's
        // traditional preprocessor (see compileCudaSplit).
        let sharedFlags =
            if caps.Platform = PWindows then "-shared -Xcompiler /Zc:preprocessor"
            else "-shared -Xcompiler -fPIC"
        let nvccArgs = $"-std=c++17 -O2 {sharedFlags} -o \"{dllFull}\" \"{cuFull}\""
        match runProc "nvcc" nvccArgs 180000 with
        | Error e -> Error e
        | Ok () -> compileCppWithExtra [dllFull] cppFile outputDir

/// Build the BLADE_LLVM lane's runtime shim into `outputDir`, returning the
/// object to link. Recompiled only when the `.o` is missing or older than the
/// `.c` EmitLlvm just deployed, so the per-program cost is one link input, not
/// a C compile.
let private buildLlvmShim (clang: string) (outputDir: string) : Result<string, string> =
    let dir = Path.GetFullPath outputDir
    let src = Path.Combine(dir, EmitLlvm.shimFileName)
    let obj = Path.Combine(dir, Path.GetFileNameWithoutExtension EmitLlvm.shimFileName + Platforms.objExtension)
    if not (File.Exists src) then
        Error $"llvm shim source missing at {src} (EmitLlvm.deployShim should have written it)"
    elif File.Exists obj && File.GetLastWriteTimeUtc obj >= File.GetLastWriteTimeUtc src then
        Ok obj
    else
        let args = $"-c -O2 -o \"{obj}\" \"{src}\""
        match runProc clang args 120000 with
        | Error e -> Error e
        | Ok () -> Ok obj

/// Identity stamp for one clang binary without spawning it: path, size,
/// mtime ticks. A replaced or upgraded clang changes the stamp — the same
/// stamp-not-hash choice `linkedDllStamp` makes for toolchain binaries.
let private clangStamp (clang: string) : string =
    try
        let fi = System.IO.FileInfo(clang)
        if fi.Exists then $"{clang}:{fi.Length}:{fi.LastWriteTimeUtc.Ticks}"
        else $"{clang}:missing"
    with _ -> $"{clang}:?"

/// The LLVM lane's twin of `exeCacheKey` (Stage 4.1). Differences, each
/// deliberate: the compiler identity is the clang STAMP, not `gppIdentity`;
/// the runtime input is the shim SOURCE text (the `.o` is a link input whose
/// content the argument string cannot see); and the version tag is its own,
/// so the two lanes can never collide on a key.
let private llvmExeCacheKey (clang: string) (args: string) (llText: string) (shimText: string) (exeFullPath: string) (llFullPath: string) : string =
    let normalizedArgs = args.Replace(exeFullPath, "<EXE>").Replace(llFullPath, "<LL>")
    let material =
        String.concat " "
            [ "blade-llvm-exe-cache-v1"
              clangStamp clang
              normalizedArgs
              shimText
              llText ]
    use sha = System.Security.Cryptography.SHA256.Create()
    sha.ComputeHash(System.Text.Encoding.UTF8.GetBytes material)
    |> Array.map _.ToString("x2")
    |> String.concat ""

/// Compile and link a `.ll` the LLVM lane emitted into an executable.
///
/// ONE clang invocation does the whole back half -- textual IR in, native
/// executable out (probe P2 in the plan: no `opt`/`llc` orchestration is
/// needed, and none is wanted, since every fact the lane emits is designed for
/// the stock -O3 pipeline). `-Wno-override-module` silences the one benign
/// warning the deliberate absence of a target triple produces.
///
/// Cached like the g++ lane (same directory, same BLADE_EXE_CACHE gate, same
/// eviction) so a rebuild of unchanged source is a file copy on both lanes —
/// the plan's "no exe cache inverts the 4.5x on warm rebuilds" gap, closed.
let compileLlvmProgram (llFile: string) (outputDir: string) : Result<string, string> =
    match resolveClang () with
    | None -> Error "Skipped: BLADE_LLVM is on but no clang was found (set BLADE_LLVM_CLANG)"
    | Some clang ->
        let dir = Path.GetFullPath outputDir
        let llFull = Path.GetFullPath llFile
        let exeFull = Path.Combine(dir, Path.GetFileNameWithoutExtension llFile + Platforms.exeExtension)
        match buildLlvmShim clang dir with
        | Error e -> Error e
        | Ok shimObj ->
            let args =
                $"{(llvmOptFlags ())} -Wno-override-module -o \"{exeFull}\" \"{llFull}\" \"{shimObj}\""
            // Windows-only for the same reason as the g++ arm; a key that
            // cannot be built (unreadable .ll/shim source) just skips the
            // cache, never the compile.
            let cacheSlot =
                if Platforms.os <> Platforms.Windows then None
                else
                    match exeCacheDir () with
                    | None -> None
                    | Some cdir ->
                        try
                            let llText = File.ReadAllText llFull
                            let shimText = File.ReadAllText (Path.Combine(dir, EmitLlvm.shimFileName))
                            Some (cdir, llvmExeCacheKey clang args llText shimText exeFull llFull)
                        with _ -> None
            match cacheSlot with
            | Some (cdir, key) when tryExeCacheHit cdir key exeFull -> Ok exeFull
            | _ ->
                match runProc clang args 180000 with
                | Error e -> Error e
                | Ok () ->
                    (match cacheSlot with
                     | Some (cdir, key) -> storeExeCache cdir key exeFull
                     | None -> ())
                    Ok exeFull

/// Compiles a generated source file according to its backend requirement,
/// resolved against the environment's capabilities. A skip is reported as
/// `Error "Skipped: <reason>"` so downstream skip handling recognizes it.
/// `srcText` is the generated source the caller just wrote to `srcFile`, when
/// it still holds it; the g++ arm uses it instead of reading the file back.
let compileForBackendSource (srcText: string option) (caps: Capabilities) (req: BackendReq) (srcFile: string) (outputDir: string) : Result<string, string> =
    match resolveCompile caps req with
    | UseGpp          -> compileCppSource srcText srcFile outputDir
    // ASan cannot instrument device code, and nvcc's host-side ASan story on
    // Windows is unsupported; a memcheck run of a CUDA-emitting program is a
    // skip, not a silently-uninstrumented build.
    | UseNvcc when CodeGen.memcheckEnabled () ->
        Error "Skipped: memcheck does not support the CUDA backend"
    | UseNvcc         -> compileCuda srcFile outputDir
    | SkipCompile why -> Error ("Skipped: " + why)

/// `compileForBackendSource` for a caller without the source in memory.
let compileForBackend (caps: Capabilities) (req: BackendReq) (srcFile: string) (outputDir: string) : Result<string, string> =
    match resolveCompile caps req with
    | UseGpp          -> compileCpp srcFile outputDir
    // ASan cannot instrument device code, and nvcc's host-side ASan story on
    // Windows is unsupported; a memcheck run of a CUDA-emitting program is a
    // skip, not a silently-uninstrumented build.
    | UseNvcc when CodeGen.memcheckEnabled () ->
        Error "Skipped: memcheck does not support the CUDA backend"
    | UseNvcc         -> compileCuda srcFile outputDir
    | SkipCompile why -> Error ("Skipped: " + why)


/// Windows: make a launched program resolve netcdf.dll's dependency set from
/// the NetCDF install itself. A provider program links netcdf dynamically;
/// the loader finds netcdf.dll via PATH but resolves its own imports (its
/// bundled zlib1.dll among them) through the same PATH order, where MSYS2
/// ucrt64 -- ahead of the install because g++ needs it -- ships a shadowing
/// zlib1.dll, and the exe dies at startup with STATUS_ENTRYPOINT_NOT_FOUND
/// before main. Prepending <NETCDF_DIR>/bin lets the install's own
/// dependency set win. Read per call, like the other environment gates.
let private prependNetcdfBin (psi: ProcessStartInfo) =
    if Platforms.os = Platforms.Windows then
        match Toolchain.get "NETCDF_DIR" with
        | Some dir when dir <> "" ->
            let bin = Path.Combine(dir, "bin")
            if Directory.Exists bin then
                let cur = match psi.Environment.TryGetValue "PATH" with | true, v -> v | _ -> ""
                psi.Environment.["PATH"] <- bin + ";" + cur
        | _ -> ()

/// Run a compiled executable
let runExecutable (exeFile: string) : Result<int * string, string> =
    try
        let exeFullPath = Path.GetFullPath(exeFile)
        let psi = ProcessStartInfo(exeFullPath)
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.UseShellExecute <- false
        psi.CreateNoWindow <- true
        psi.WorkingDirectory <- Path.GetDirectoryName(exeFullPath)
        prependNetcdfBin psi
        
        use proc = Process.Start(psi)
        // Drain both streams on dedicated threads -- never the thread pool (Runtime.readToEndOffPool)
        let stdoutTask = Blade.Runtime.readToEndOffPool proc.StandardOutput
        let stderrTask = Blade.Runtime.readToEndOffPool proc.StandardError
        
        // 120s: simulation-scale examples (thousands of spectral steps) can
        // legitimately run long; corpus tests still finish well under a second.
        // Memcheck runs get 600s: /Od plus ASan interception is a 5-20x
        // slowdown on exactly those simulation-scale programs.
        let timeoutMs = if CodeGen.memcheckEnabled () then 600000 else 120000
        if proc.WaitForExit(timeoutMs) then
            let stdout = stdoutTask.Result
            let stderr = stderrTask.Result
            let output = if String.IsNullOrEmpty(stderr) then stdout else stdout + "\n[stderr]: " + stderr
            Ok (proc.ExitCode, output)
        else
            try proc.Kill() with _ -> ()
            // WHAT IT MANAGED TO SAY, before it stopped saying anything.
            //
            // A bare "timed out after 120s" is not a diagnosis: it cannot
            // distinguish a program that hung before it printed a thing from
            // one that printed every correct value and then hung on the way
            // out, and those two have nothing in common but the symptom.
            //
            // The success path is untouched -- it still reads to end, so the
            // captured bytes stay byte-identical for the differential gates
            // that compare them. This is only reachable once the process is
            // dead, and killing it closes its pipe ends, which is exactly what
            // lets the readers complete with the partial content. Bounded, so
            // a wedged reader cannot turn a timeout into a second hang.
            let grab (t: System.Threading.Tasks.Task<string>) =
                try (if t.Wait 5000 then t.Result else "") with _ -> ""
            let describe (label: string) (text: string) =
                if String.IsNullOrWhiteSpace text then $"no {label}"
                else
                    let lines =
                        text.Replace("\r\n", "\n").Split('\n')
                        |> Array.filter (fun l -> l.Trim() <> "")
                    $"{lines.Length} line(s) of {label}, last: {(lines.[lines.Length - 1].Trim())}"
            Error (sprintf "Execution timed out after %ds (%s; %s)"
                       (timeoutMs / 1000)
                       (describe "stdout" (grab stdoutTask))
                       (describe "stderr" (grab stderrTask)))
    with ex ->
        Error $"Execution exception: {ex.Message}"

// MPI launch support (mpiexec resolution + wrapped execution)

/// Locates mpiexec. The MS-MPI installer updates the MACHINE-scope PATH,
/// which already-running processes never see, so a bare PATH lookup is the
/// last resort. Probe order: process-env MSMPI_BIN -> machine-scope
/// MSMPI_BIN -> the well-known install path -> bare "mpiexec" (marker-
/// probed; MS-MPI's exit codes are untrustworthy). Lazy: resolved once.
let mpiexecPath : Lazy<string option> =
    lazy (
        let mpiexecName = if Platforms.os = Platforms.Windows then "mpiexec.exe" else "mpiexec"
        let fromEnv (scope: EnvironmentVariableTarget option) =
            try
                let v =
                    match scope with
                    | Some s -> Environment.GetEnvironmentVariable("MSMPI_BIN", s)
                    | None -> (match Toolchain.get "MSMPI_BIN" with Some d -> d | None -> null)
                match v with
                | null | "" -> None
                | d -> Some (Path.Combine(d, mpiexecName))
            with _ -> None
        let onWindows = Platforms.os = Platforms.Windows
        [ fromEnv None
          (if onWindows then fromEnv (Some EnvironmentVariableTarget.Machine) else None)
          (if onWindows then Some @"C:\Program Files\Microsoft MPI\Bin\mpiexec.exe" else None)
          Some "mpiexec" ]
        |> List.choose id
        |> List.tryFind (fun exe ->
            if Path.IsPathRooted exe then File.Exists exe
            else probeToolLoose exe "-help" "mpi"))

/// Whether g++ can compile+link an MPI program (Platforms.mpiLinkFlag
/// resolvable -- the MSYS2 msmpi package on Windows, the OpenMPI/MPICH dev
/// package elsewhere): one real link probe in a temp dir, lazy so ordinary
/// invocations never pay for it.
let hasMpiLink : Lazy<bool> =
    lazy (
        try
            let dir = Path.Combine(Path.GetTempPath(), "blade_mpi_probe")
            Directory.CreateDirectory(dir) |> ignore
            let src = Path.Combine(dir, "mpi_probe.cpp")
            File.WriteAllText(src,
                "#include <mpi.h>\nint main(int argc, char** argv){ MPI_Init(&argc,&argv); MPI_Finalize(); return 0; }\n")
            let exe = Path.Combine(dir, "mpi_probe" + Platforms.exeExtension)
            let psi = ProcessStartInfo("g++", $"-std=c++17 \"{src}\" {Platforms.mpiLinkFlag} -o \"{exe}\"")
            psi.RedirectStandardOutput <- true
            psi.RedirectStandardError <- true
            psi.UseShellExecute <- false
            psi.CreateNoWindow <- true
            use proc = Process.Start(psi)
            proc.StandardOutput.ReadToEnd() |> ignore
            proc.StandardError.ReadToEnd() |> ignore
            proc.WaitForExit(30000) |> ignore
            proc.ExitCode = 0
        with _ -> false)

/// Run a compiled MPI executable under `mpiexec -n <ranks>`. Same
/// stream/timeout discipline as runExecutable; mpiexec propagates a failing
/// rank's exit code. 60s timeout (multi-process startup is slower than a bare exe).
let runExecutableMpi (ranks: int) (exeFile: string) : Result<int * string, string> =
    match mpiexecPath.Value with
    | None -> Error $"mpiexec not found ({Platforms.mpiRuntimeHint})"
    | Some mpiexec ->
        try
            let exeFullPath = Path.GetFullPath(exeFile)
            let psi = ProcessStartInfo(mpiexec, $"-n {ranks} \"{exeFullPath}\"")
            psi.RedirectStandardOutput <- true
            psi.RedirectStandardError <- true
            psi.UseShellExecute <- false
            psi.CreateNoWindow <- true
            psi.WorkingDirectory <- Path.GetDirectoryName(exeFullPath)
            prependNetcdfBin psi
            use proc = Process.Start(psi)
            let stdoutTask = Blade.Runtime.readToEndOffPool proc.StandardOutput
            let stderrTask = Blade.Runtime.readToEndOffPool proc.StandardError
            if proc.WaitForExit(60000) then
                let stdout = stdoutTask.Result
                let stderr = stderrTask.Result
                let output = if String.IsNullOrEmpty(stderr) then stdout else stdout + "\n[stderr]: " + stderr
                Ok (proc.ExitCode, output)
            else
                try proc.Kill() with _ -> ()
                Error "Execution timed out after 60s (mpiexec)"
        with ex ->
            Error $"Execution exception: {ex.Message}"

/// Sanitize a test name for use as a filename (cross-platform).
let sanitizeFileName (name: string) : string =
    name
        .Replace("&&", "_and_")
        .Replace("||", "_or_")
        .Replace(" ", "_")
        .Replace(":", "")
        .Replace("/", "_")
        .Replace("\\", "_")
        .Replace("(", "")
        .Replace(")", "")
        .Replace("|", "_")
        .Replace("&", "_")
        .Replace("+", "_")
        .Replace(",", "_")
        .Replace("<", "_")
        .Replace(">", "_")
        .Replace("\"", "")
        .Replace("*", "_")
        .Replace("?", "_")
