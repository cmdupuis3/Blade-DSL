// Device-buffer type unit tests and the differential CUDA kernel tests
// (cuda-vs-host codegen equivalence). Extracted verbatim from Main.fs
// (audit §2.3). Requires nvcc + a CUDA GPU (+ cl.exe on Windows, i.e. the
// "x64 Native Tools" prompt); skips cleanly otherwise.
module Blade.Tests.CudaTests

open System
open Blade
open System.IO
open System.Diagnostics
open System.Runtime.InteropServices
open Blade.Ast
open Blade.IR
open Blade.IRLoopStructure
open Blade.IRStorage
open Blade.IRLift
open Blade.IRMono
open Blade.IRPrint
open Blade.IRValidate
open Blade.Types
open Blade.Lowering
open Blade.CodeGen
open Blade.Build
open Blade.Tests.TestHarness

/// F# unit tests for the DeviceBufferType dimensional-type machinery (the
/// foundation for CUDA buffer streaming). Verifies cardinality computation —
/// the load-bearing arithmetic that, if wrong, would silently corrupt the
/// device buffer mapping (and there is no CPU oracle once on hardware, so this
/// must be checked HERE against hand-computed values). Pure F#, no g++.
let runBufferTypeTests () : Blade.Tests.TestHarness.BlockResult =
    printHeader "Device Buffer Type Tests"
    let mutable failures = 0
    let mutable passed = 0
    let mutable failedNames = []
    let lit n = IRLit (IRLitInt (int64 n))
    // A buffer dim group constructor for the test
    let grp rank ext symm : BufferDimGroup =
        { Rank = rank; Extent = lit ext; Symmetry = symm
          Kind = (if symm = SymNone then TDimension else SDimension)
          Dependencies = [] }
    // Rectangular SDimension group (Rank 1, SymNone, but SDimension)
    let rectS ext : BufferDimGroup =
        { Rank = 1; Extent = lit ext; Symmetry = SymNone
          Kind = SDimension; Dependencies = [] }
    let card (groups: BufferDimGroup list) =
        match deviceBufferCardinality { ElemType = IRTScalar ETFloat64; Groups = groups } with
        | IRLit (IRLitInt n) -> Some n
        | _ -> None
    let pass name detail =
        passed <- passed + 1
        Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Pass name detail
    let fail name detail =
        failures <- failures + 1
        failedNames <- failedNames @ [name]
        Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail name detail
    let check name (groups: BufferDimGroup list) (expected: int64) =
        match card groups with
        | Some n when n = expected ->
            pass name ($"=> {n}")
        | Some n ->
            fail name ($"=> {n} (expected {expected})")
        | None ->
            fail name ($"=> non-literal (expected {expected})")
    // Rectangular: 8 x 8 = 64
    check "rect 8x8" [rectS 8; rectS 8] 64L
    // Rectangular 1-D: 12
    check "rect 12" [rectS 12] 12L
    // Symmetric SymIdx<2> over n=5: C(5+2-1, 2) = C(6,2) = 15
    check "sym2 n=5" [grp 2 5 SymSymmetric] 15L
    // Symmetric SymIdx<3> over n=4: C(4+3-1,3) = C(6,3) = 20
    check "sym3 n=4" [grp 3 4 SymSymmetric] 20L
    // Antisymmetric AntisymIdx<2> over n=5: C(5,2) = 10
    check "antisym2 n=5" [grp 2 5 SymAntisymmetric] 10L
    // Antisymmetric AntisymIdx<3> over n=5: C(5,3) = 10
    check "antisym3 n=5" [grp 3 5 SymAntisymmetric] 10L
    // Hermitian = same storage count as symmetric: C(6,2) = 15
    check "herm2 n=5" [grp 2 5 SymHermitian] 15L
    // Product symmetry: symmetric(n=5,r=2)=15 times rectangular 4 = 60
    check "sym2 x rect4" [grp 2 5 SymSymmetric; rectS 4] 60L
    // isRectangularConstBuffer predicate
    let rectBt = { ElemType = IRTScalar ETFloat64; Groups = [rectS 8; rectS 8] }
    let symBt = { ElemType = IRTScalar ETFloat64; Groups = [grp 2 5 SymSymmetric] }
    if isRectangularConstBuffer rectBt then pass "isRectangular(rect)" "true"
    else fail "isRectangular(rect)" "should be true"
    if not (isRectangularConstBuffer symBt) then pass "isRectangular(sym)" "false"
    else fail "isRectangular(sym)" "should be false"
    // extern "C" boundary-safety gate: fundamental scalars cross, library types don't.
    let checkBnd name ty expected =
        if isCudaBoundarySafeElem ty = expected then pass ($"boundary({name})") (sprintf "%b" expected)
        else fail ($"boundary({name})") (sprintf "should be %b" expected)
    checkBnd "f64" (IRTScalar ETFloat64) true
    checkBnd "f32" (IRTScalar ETFloat32) true
    checkBnd "i64" (IRTScalar ETInt64) true
    checkBnd "i32" (IRTScalar ETInt32) true
    checkBnd "bool" (IRTScalar ETBool) true
    // Complex crosses since the 2026-07-19 complex-over-CUDA arc: std::complex
    // signatures at the extern "C" boundary, thrust::complex device dialect
    // inside the .cu (both layout-compatible with T[2]).
    checkBnd "complex128" (IRTScalar ETComplex128) true
    checkBnd "complex64" (IRTScalar ETComplex64) true
    checkBnd "string" (IRTScalar ETString) false
    printFooter "Buffer Type" [$"{passed} passed"; $"{failures} failure(s)"]
    { Block = "Buffer Type"; Passed = passed; Failed = failures; Skipped = 0; FailedNames = failedNames }

/// First `where cuda` hardware test. Differential: generate the SAME rank-1 map
/// program twice — once WITHOUT a cuda clause (host-loop oracle, g++), once WITH
/// `cuda(block: N)` (device kernel, split-compiled nvcc+g++ then linked) — run
/// both, and require identical output. This verifies cuda-vs-host CODEGEN
/// equivalence (not just cuda-vs-hand-math). SKIPs cleanly when nvcc/GPU absent,
/// so the harness stays green on non-CUDA machines; runs for real where a GPU is
/// present. Rank-1 is the simplest kernel: flat thread index IS the coordinate
/// (no div/mod recovery).
let runCudaTests () : Blade.Tests.TestHarness.BlockResult =
    printHeader "CUDA Kernel Tests"
    // Skipped = 1, not 0: with Skipped = 0 a GPU-less box printed
    // "0 passed, 0 failed" for this block, which reads as "nothing to do"
    // rather than "the environment cannot run this". Same convention as
    // DiffOracle/InterpDiff. The reason is printed by each branch below.
    let skipResult = { Block = "CUDA Kernel"; Passed = 0; Failed = 0; Skipped = 1; FailedNames = [] }
    let caps = capabilities.Value
    let onWindows = RuntimeInformation.IsOSPlatform(OSPlatform.Windows)
    if not caps.HasNvcc || not caps.HasGpu then
        printfn "Skipped: requires nvcc + CUDA GPU (nvcc=%b, gpu=%b)." caps.HasNvcc caps.HasGpu
        skipResult  // skip, not failure — mirrors harness skip policy
    elif (not onWindows) && not caps.HasGpp then
        // g++ is the host compiler only on the Linux path; Windows uses nvcc/cl.
        printfn "Skipped: requires g++ for the host half (Linux path)."
        skipResult
    elif onWindows && not caps.HasCl then
        // On Windows nvcc drives MSVC's cl.exe as its host compiler. If cl.exe
        // isn't on PATH (e.g. running from a plain terminal rather than the
        // "x64 Native Tools Command Prompt for VS"), nvcc fails with
        // "Cannot find compiler 'cl.exe'". Skip cleanly here — same policy as
        // resolveCompile's RequiresCuda/PWindows/not-HasCl branch — rather than
        // attempt a compile that's guaranteed to fail. To actually run this
        // test on Windows, launch from the VS Native Tools prompt (or run
        // vcvars64.bat first) so cl.exe is on PATH.
        printfn "Skipped: nvcc needs cl.exe (MSVC) as host compiler, not found on PATH."
        printfn "         Run from the 'x64 Native Tools Command Prompt for VS' (or after vcvars64.bat)."
        skipResult
    else
        let outputDir = "./generated_cpp_tests"
        Directory.CreateDirectory(outputDir) |> ignore
        CodeGen.deployRuntimeHeaders outputDir

        // Compile a plain host .cpp (the oracle) without MinGW on Windows: there
        // we use nvcc to drive cl.exe (single MSVC toolchain, consistent with the
        // cuda variant's host half). On Linux, g++ via compileCpp.
        let compileHost (cppFile: string) : Result<string, string> =
            if onWindows then
                let cppFull = Path.GetFullPath(cppFile)
                let exeFull = Path.ChangeExtension(cppFull, ".exe")
                // Stays at -O2, NOT Build.optFlags: this is an nvcc invocation
                // (cl.exe host), and nvcc's host-flag translation does not take
                // -march cleanly on Windows. Same rule as Build.fs's CUDA paths.
                // The Linux branch below goes through compileCpp, so it DOES
                // pick up the shared -O3 -march flags.
                let args = $"-std=c++17 -O2 -o \"{exeFull}\" \"{cppFull}\""
                runProc "nvcc" args 120000 |> Result.map (fun () -> exeFull)
            else compileCpp cppFile outputDir

        // Generate one variant: lower -> validate -> codegen -> write .cpp (+ .cu
        // if a kernel was emitted). Returns (cppFile, optional cuFile).
        let genVariant (name: string) (src: string) : Result<string * string option, string> =
            try
                match lower src with
                | Error e -> Error ($"lower failed: {e}")
                | Ok ir0 ->
                    // Hard-fail on validation errors instead of generating from
                    // invalid IR (was `| Error _ -> ir0`). The host/device
                    // differential compares two variants; if BOTH were generated
                    // from invalid IR they could still agree and pass.
                    match IRValidate.validateIR ir0 with
                    | Error validationErrors ->
                        Error ($"""IR validation failed: {(String.concat "; " validationErrors)}""")
                    | Ok ir ->
                    let (cppCode, _w) = CodeGen.genSelfContainedProgramFromIR ir name
                    let cuOpt = CodeGen.getCudaFileContent ()
                    let safe = sanitizeFileName name
                    let cppFile = Path.Combine(outputDir, safe + ".cpp")
                    File.WriteAllText(cppFile, cppCode)
                    let cuFileOpt =
                        cuOpt |> Option.map (fun cu ->
                            let cuFile = Path.Combine(outputDir, safe + ".cu")
                            File.WriteAllText(cuFile, cu)
                            cuFile)
                    Ok (cppFile, cuFileOpt)
            with ex -> Error ($"codegen failed: {ex.Message}")

        // runExecutable returns (exitCode, output). The three call sites below
        // used `|> Result.map snd`, which DISCARDED the exit code: a process
        // that crashed before printing anything yielded Ok "" — so the
        // host-vs-device differential compared "" with "" and PASSED, and the
        // host-only case's `.Contains expectSubstr` was the only gate on a
        // program that may never have run. Require exit 0 explicitly. (The
        // thrust/complex case at the bottom of this file already does this;
        // this makes every call site agree.)
        let runExeChecked (what: string) (exe: string) : Result<string, string> =
            match runExecutable exe with
            | Error e -> Error ($"{what} run: {e}")
            | Ok (0, out) -> Ok out
            | Ok (code, out) -> Error ($"{what} exited {code}:\n{out}")

        let resultLines (s: string) =
            (s.Replace("\r\n", "\n").Trim()).Split('\n')
            |> Array.filter (fun l -> not (l.Contains("completed in")))
            |> String.concat "\n"

        // One differential case: the SAME program with and without `where cuda`.
        // host (no clause, must emit NO .cu) vs cuda (clause, must emit a .cu);
        // both run, outputs must match (cuda-vs-host codegen equivalence).
        // Returns 0 on pass, 1 on failure; prints a labeled line either way.
        let runCudaCase (label: string) (hostSrc: string) (cudaSrc: string) : int =
            let hostName = $"cuda_{label}_host"
            let cudaName = $"cuda_{label}_dev"
            // Force-clean stale artifacts for THIS case (the generated dir
            // persists across runs / version unzips).
            for stem in [hostName; cudaName] do
                for ext in [".cu"; ".cpp"; ".cu.obj"; ".cpp.obj"; ".cu.o"; ".cpp.o"; ".exe"; ".out"] do
                    let f = Path.Combine(outputDir, stem + ext)
                    try if File.Exists f then File.Delete f with _ -> ()
            let hostOut =
                // Host variant: CUDA emission OFF -> `cuda` clause stays inert,
                // no .cu emitted, pure host-loop codegen (the oracle).
                CodeGen.setCudaEmitMode false
                match genVariant hostName hostSrc with
                | Error e -> Error e
                | Ok (cppFile, cuOpt) ->
                    if cuOpt.IsSome then Error "host variant unexpectedly emitted a .cu"
                    else
                        match compileHost cppFile with
                        | Error e -> Error ($"host compile: {e}")
                        | Ok exe -> runExeChecked "host" exe
            let cudaOut =
                // CUDA variant: emission ON -> kernel + launch emitted into .cu/.cpp.
                CodeGen.setCudaEmitMode true
                let r =
                    match genVariant cudaName cudaSrc with
                    | Error e -> Error e
                    | Ok (_cppFile, None) -> Error "cuda variant did not emit a .cu (kernel not generated)"
                    | Ok (cppFile, Some cuFile) ->
                        // SELF-CONTAINMENT. The .cu is its own translation unit:
                        // it may name its parameters and device helpers, never a
                        // HOST identifier. A lifted kernel body (`__lambda_N`) or
                        // a codegen sentinel reaching it is the emitter promising
                        // a device kernel it cannot write -- nvcc reports it as
                        // "identifier ... is undefined" only AFTER the host half
                        // has been told the kernel exists, so pin it here where
                        // the message names the actual cause.
                        let cu = File.ReadAllText cuFile
                        if cu.Contains "__lambda_" then
                            Error "the .cu names a lifted HOST function (__lambda_*) -- the capability gate let through a body it cannot emit"
                        elif cu.Contains "BLADE_CODEGEN_ERROR" then
                            Error "the .cu contains a codegen sentinel"
                        else
                        match compileCudaSplit cuFile cppFile outputDir with
                        | Error e -> Error ($"cuda split-compile: {e}")
                        | Ok exe -> runExeChecked "cuda" exe
                CodeGen.setCudaEmitMode false
                r
            match hostOut, cudaOut with
            | Error e, _ -> Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail label ($"host oracle: {e}"); 1
            | _, Error e -> Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail label ($"cuda: {e}"); 1
            | Ok hOut, Ok cOut ->
                let h, c = resultLines hOut, resultLines cOut
                // Both variants print a result line; "" = "" would otherwise be
                // a vacuous agreement (e.g. if both programs became silent).
                if String.IsNullOrWhiteSpace h || String.IsNullOrWhiteSpace c then
                    Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail label
                        ($"empty output -- nothing to compare (host={h.Length} chars, cuda={c.Length} chars)")
                    1
                elif h = c then
                    Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Pass label "cuda matches host-loop oracle"
                    0
                else
                    Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail label "cuda output differs from host oracle"
                    printfn "    host: %s" h
                    printfn "    cuda: %s" c
                    1

        // Host-only compile+run check (no cuda variant). Used for cases that
        // aren't cuda-eligible but exercise a host codegen path we want to keep
        // honest under MSVC — notably a genuinely SYMMETRIC output, which fires
        // the hasRealSymmetry=true branch (named static symm array passed to
        // allocate). Verifies the program compiles under cl AND its result line
        // contains the expected substring.
        let runHostCompileCase (label: string) (src: string) (expectSubstr: string) : int =
            let nm = $"cuda_{label}_host"
            for ext in [".cu"; ".cpp"; ".cu.obj"; ".cpp.obj"; ".cu.o"; ".cpp.o"; ".exe"; ".out"] do
                let f = Path.Combine(outputDir, nm + ext)
                try if File.Exists f then File.Delete f with _ -> ()
            let outcome =
                match genVariant nm src with
                | Error e -> Error e
                | Ok (cppFile, _) ->
                    match compileHost cppFile with
                    | Error e -> Error ($"host compile: {e}")
                    | Ok exe -> runExeChecked "host" exe
            match outcome with
            | Error e -> Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail label e; 1
            | Ok out ->
                let r = resultLines out
                if r.Contains(expectSubstr) then
                    Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Pass label "host compiles under MSVC + correct result"
                    0
                else
                    Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail label ($"expected substring {expectSubstr} not in output")
                    printfn "    output: %s" r
                    1

        // A case = (label, hostSrc, cudaSrc). cudaSrc adds `where cuda(block: N)`
        // to the lambda; everything else identical so any diff is a codegen bug.
        // Source variants bound individually first. Multi-line triple-quoted
        // strings whose content dedents to column 0 confuse F#'s layout analysis
        // when placed directly inside a list-of-tuples literal (the column-0
        // `let A = ...` inside the string collides with the enclosing block's
        // offside context). Binding them to names first sidesteps that entirely.
        let rank1Host = """
let A = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]
let R = method_for(A) <@> lambda(x) -> x * 2.0 + 1.0 |> compute
"""
        let rank1Cuda = """
let A = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]
let R = method_for(A) <@> lambda(x) where cuda(block: 64) -> x * 2.0 + 1.0 |> compute
"""
        // rank-2 outer product: exercises div/mod coordinate recovery AND
        // two-input streaming (the parts rank-1 skips entirely).
        let rank2Host = """
let A = [1.0, 2.0, 3.0]
let B = [4.0, 5.0, 6.0]
let R = method_for(A, B) <@> lambda(x, y) -> x * y |> compute
"""
        let rank2Cuda = """
let A = [1.0, 2.0, 3.0]
let B = [4.0, 5.0, 6.0]
let R = method_for(A, B) <@> lambda(x, y) where cuda(block: 64) -> x * y |> compute
"""
        // multi-block: 8 elements with block size 4 => 2 blocks, so the grid
        // spans multiple blocks (stresses __blade_blocks math + the bound check).
        let mbHost = """
let A = [10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0]
let R = method_for(A) <@> lambda(x) -> x * 3.0 |> compute
"""
        let mbCuda = """
let A = [10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0]
let R = method_for(A) <@> lambda(x) where cuda(block: 4) -> x * 3.0 |> compute
"""
        // rank-3 with NON-UNIFORM extents (3x2x3=18): two `/=` steps in the
        // coordinate recovery with DISTINCT moduli per dim — the deepest test of
        // the div/mod unpacking (rank-2 used equal extents, which is more forgiving).
        let rank3Host = """
let A = [1.0, 2.0, 3.0]
let B = [10.0, 20.0]
let C = [100.0, 200.0, 300.0]
let R = method_for(A, B, C) <@> lambda(a, b, c) -> a + b + c |> compute
"""
        let rank3Cuda = """
let A = [1.0, 2.0, 3.0]
let B = [10.0, 20.0]
let C = [100.0, 200.0, 300.0]
let R = method_for(A, B, C) <@> lambda(a, b, c) where cuda(block: 64) -> a + b + c |> compute
"""
        // integer element type: exercises int64_t crossing the extern "C"
        // boundary (boundary-safe set includes ETInt64), not just double.
        let intHost = """
let A = [1, 2, 3, 4, 5, 6]
let R = method_for(A) <@> lambda(x) -> x * 10 |> compute
"""
        let intCuda = """
let A = [1, 2, 3, 4, 5, 6]
let R = method_for(A) <@> lambda(x) where cuda(block: 64) -> x * 10 |> compute
"""
        // non-trivial kernel body: a polynomial in x exercises a deeper
        // exprToCpp expression in the kernel (multiple ops, reuse of the bound
        // element), not just a single multiply.
        let polyHost = """
let A = [1.0, 2.0, 3.0, 4.0, 5.0]
let R = method_for(A) <@> lambda(x) -> x * x + x * 2.0 - 1.0 |> compute
"""
        let polyCuda = """
let A = [1.0, 2.0, 3.0, 4.0, 5.0]
let R = method_for(A) <@> lambda(x) where cuda(block: 64) -> x * x + x * 2.0 - 1.0 |> compute
"""
        // larger grid: 50 elements, block 8 => 7 blocks. Bigger grid than the
        // 2-block mb case; the last block is partial (50 = 6*8 + 2), so the
        // bound check is genuinely exercised at a non-trivial tail.
        let bigHost = """
let A = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, 20.0, 21.0, 22.0, 23.0, 24.0, 25.0, 26.0, 27.0, 28.0, 29.0, 30.0, 31.0, 32.0, 33.0, 34.0, 35.0, 36.0, 37.0, 38.0, 39.0, 40.0, 41.0, 42.0, 43.0, 44.0, 45.0, 46.0, 47.0, 48.0, 49.0, 50.0]
let R = method_for(A) <@> lambda(x) -> x + 100.0 |> compute
"""
        let bigCuda = """
let A = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, 20.0, 21.0, 22.0, 23.0, 24.0, 25.0, 26.0, 27.0, 28.0, 29.0, 30.0, 31.0, 32.0, 33.0, 34.0, 35.0, 36.0, 37.0, 38.0, 39.0, 40.0, 41.0, 42.0, 43.0, 44.0, 45.0, 46.0, 47.0, 48.0, 49.0, 50.0]
let R = method_for(A) <@> lambda(x) where cuda(block: 8) -> x + 100.0 |> compute
"""
        // SYMMETRIC output (host-only; cuda gates reject non-rectangular). The
        // `comm(x, y)` on the kernel + same array A twice folds into a SYMMETRIC
        // output (SymIdx), so OutputSymmVec has a repeated group => hasRealSymmetry
        // is TRUE => the named-static symm-array allocate branch fires. This is the
        // branch the v24 fix did NOT change (the else), so this case confirms it
        // still compiles under MSVC and produces correct values. method_for(A, A)
        // with distinct... no: SAME array A is required for the comm to fold.
        let symHost = """
let A = [1.0, 2.0, 3.0, 4.0]
let R = method_for(A, A) <@> lambda(x, y) where comm(x, y) -> x * y |> compute
"""
        // SYMMETRIC rank-2 DIFFERENTIAL case (the first triangular CUDA path):
        // same symmetric Reynolds product with vs without `where cuda`. The cuda
        // variant must emit a .cu (genCudaKernelSimplicial fires: single arity-2
        // symmetric group, const square extent, symmetric — not antisym — fold),
        // run on the device with the triangular unrank, and match the host
        // triangular output exactly. A kernel that DOES touch the symmetry path.
        let symTriHost = """
let A = [1.0, 2.0, 3.0, 4.0, 5.0]
let R = method_for(A, A) <@> lambda(x, y) where comm(x, y) -> 2.0 * x + y |> compute
"""
        let symTriCuda = """
let A = [1.0, 2.0, 3.0, 4.0, 5.0]
let R = method_for(A, A) <@> lambda(x, y) where comm(x, y), cuda(block: 32) -> 2.0 * x + y |> compute
"""
        // ANTISYMMETRIC rank-2 strict-triangular DIFFERENTIAL case. reynolds(g,
        // Antisymmetric) folds to g(x,y)-g(y,x); stored on the strict triangle
        // (i<j). The cuda variant must emit a .cu (genCudaKernelSimplicial fires:
        // single arity-2 antisym group, antisym Reynolds), run the strict unrank
        // + sign on device, and match the host strict-triangular output. Lands on
        // the strict-offset/sign bug class the differential harness guards.
        let antiTriHost = """
let A = [1.0, 2.0, 3.0, 4.0, 5.0]
let g = lambda(x, y) where comm(x, y) -> x * x * y
let R = method_for(A, A) <@> reynolds(g, Antisymmetric) |> compute
"""
        let antiTriCuda = """
let A = [1.0, 2.0, 3.0, 4.0, 5.0]
let g = lambda(x, y) where comm(x, y), cuda(block: 32) -> x * x * y
let R = method_for(A, A) <@> reynolds(g, Antisymmetric) |> compute
"""
        // SYMMETRIC rank-3 INCLUSIVE simplex (i<=j<=k) DIFFERENTIAL case — the
        // first higher-rank triangular CUDA path (2-level simplicial unrank). raw
        // comm kernel over method_for(A,A,A); cuda variant must emit a .cu
        // (genCudaKernelSimplicial fires: single arity-3 symmetric group), run the
        // closed-form outer unrank + rank-2 inner unrank on device, match host.
        let sym3Host = """
let A = [1.0, 2.0, 3.0, 4.0, 5.0]
let R = method_for(A, A, A) <@> lambda(x, y, z) where comm(x, y, z) -> x + 2.0 * y + 3.0 * z |> compute
"""
        let sym3Cuda = """
let A = [1.0, 2.0, 3.0, 4.0, 5.0]
let R = method_for(A, A, A) <@> lambda(x, y, z) where comm(x, y, z), cuda(block: 32) -> x + 2.0 * y + 3.0 * z |> compute
"""
        // ANTISYMMETRIC rank-3 STRICT simplex (i<j<k) DIFFERENTIAL case — the
        // strict higher-rank path (binomial outer start + sign). non-degenerate
        // kernel x*x*y+z (antisymmetrizes to distinct nonzero values).
        let anti3Host = """
let A = [1.0, 2.0, 3.0, 4.0, 5.0]
let g = lambda(x, y, z) where comm(x, y, z) -> x * x * y + z
let R = method_for(A, A, A) <@> reynolds(g, Antisymmetric) |> compute
"""
        let anti3Cuda = """
let A = [1.0, 2.0, 3.0, 4.0, 5.0]
let g = lambda(x, y, z) where comm(x, y, z), cuda(block: 32) -> x * x * y + z
let R = method_for(A, A, A) <@> reynolds(g, Antisymmetric) |> compute
"""
        // RANK-4 and RANK-5 cases — exercise the GENERAL simplicial unrank at
        // higher S-group arity (the depths a closed-form would not cover). sym
        // = inclusive simplex, anti = strict simplex. Non-degenerate kernels.
        let sym4Host = """
let A = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
let R = method_for(A, A, A, A) <@> lambda(w, x, y, z) where comm(w, x, y, z) -> w + 2.0 * x + 3.0 * y + 4.0 * z |> compute
"""
        let sym4Cuda = """
let A = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
let R = method_for(A, A, A, A) <@> lambda(w, x, y, z) where comm(w, x, y, z), cuda(block: 32) -> w + 2.0 * x + 3.0 * y + 4.0 * z |> compute
"""
        let anti4Host = """
let A = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
let g = lambda(w, x, y, z) where comm(w, x, y, z) -> x * y * y * z * z * z
let R = method_for(A, A, A, A) <@> reynolds(g, Antisymmetric) |> compute
"""
        let anti4Cuda = """
let A = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
let g = lambda(w, x, y, z) where comm(w, x, y, z), cuda(block: 32) -> x * y * y * z * z * z
let R = method_for(A, A, A, A) <@> reynolds(g, Antisymmetric) |> compute
"""
        let sym5Host = """
let A = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
let R = method_for(A, A, A, A, A) <@> lambda(a, b, c, d, e) where comm(a, b, c, d, e) -> a + 2.0 * b + 3.0 * c + 4.0 * d + 5.0 * e |> compute
"""
        let sym5Cuda = """
let A = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
let R = method_for(A, A, A, A, A) <@> lambda(a, b, c, d, e) where comm(a, b, c, d, e), cuda(block: 32) -> a + 2.0 * b + 3.0 * c + 4.0 * d + 5.0 * e |> compute
"""
        let anti5Host = """
let A = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
let g = lambda(a, b, c, d, e) where comm(a, b, c, d, e) -> b * c * c * d * d * d * e * e * e * e
let R = method_for(A, A, A, A, A) <@> reynolds(g, Antisymmetric) |> compute
"""
        let anti5Cuda = """
let A = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
let g = lambda(a, b, c, d, e) where comm(a, b, c, d, e), cuda(block: 32) -> b * c * c * d * d * d * e * e * e * e
let R = method_for(A, A, A, A, A) <@> reynolds(g, Antisymmetric) |> compute
"""
        // Arc-8 probe: the arc-1 FUSED JOINT level (one identity group over a
        // 2-D array -> single compound axis, joint SymIdx<2, 6> output) under
        // the cuda clause. Device-side element access must decode per-dim
        // coordinates from the compound index (row-major) exactly like the
        // host fused arm — or the emitter must decline cleanly to the host
        // loop. Either way host and cuda variants must agree on values.
        let joint2dHost = """
let A: Array<Float64 like Idx<2>, Idx<3>> = [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]
let R = method_for(A, A) <@> lambda(x, y) where comm(x, y) -> x * y |> compute
"""
        let joint2dCuda = """
let A: Array<Float64 like Idx<2>, Idx<3>> = [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]
let R = method_for(A, A) <@> lambda(x, y) where comm(x, y), cuda(block: 32) -> x * y |> compute
"""
        // CO-FUSION: two SAME-ARITY cuda leaves over the SAME input, <&!>-fused
        // into ONE device launch (genCudaCoFusion). The host oracle fuses them
        // into one serial nest; the cuda variant emits a single __global__ with
        // two output buffers. Values must match. rank-1 (flat grid, shared input
        // loaded once) and rank-2 (div/mod recovery shared across both writes).
        let cofuse1Host = """
let A = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]
let (u, v) = (method_for(A) <@> lambda(x) -> x * 2.0 + 1.0) <&!> (method_for(A) <@> lambda(x) -> x + 100.0) |> compute
"""
        let cofuse1Cuda = """
let A = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]
let (u, v) = (method_for(A) <@> lambda(x) where cuda(block: 64) -> x * 2.0 + 1.0) <&!> (method_for(A) <@> lambda(x) where cuda(block: 64) -> x + 100.0) |> compute
"""
        let cofuse2Host = """
let A = [1.0, 2.0, 3.0]
let B = [4.0, 5.0, 6.0]
let (p, q) = (method_for(A, B) <@> lambda(x, y) -> x * y) <&!> (method_for(A, B) <@> lambda(x, y) -> x + y) |> compute
"""
        let cofuse2Cuda = """
let A = [1.0, 2.0, 3.0]
let B = [4.0, 5.0, 6.0]
let (p, q) = (method_for(A, B) <@> lambda(x, y) where cuda(block: 64) -> x * y) <&!> (method_for(A, B) <@> lambda(x, y) where cuda(block: 64) -> x + y) |> compute
"""
        // <&> SOFT JOIN over independent cuda leaves: leaves that cannot
        // co-fuse (different extents / block sizes / arities) still run as
        // per-leaf kernels, launched via split begin/end wrappers with
        // round-robin device assignment inside the .cu (one device => the
        // default stream serializes; more => the begin pass overlaps). The
        // SAME source serves as its own host oracle (clauses inert gate-off).
        let softRect = """
let A = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
let B = [10.0, 20.0, 30.0, 40.0]
let (u, v) = (method_for(A) <@> lambda(x) where cuda(block: 64) -> x * 2.0) <&> (method_for(B) <@> lambda(y) where cuda(block: 32) -> y + 100.0) |> compute
"""
        let softSimp = """
let A = [1.0, 2.0, 3.0, 4.0, 5.0]
let B = [2.0, 4.0, 6.0]
let (m2a, m2b) = (method_for(A, A) <@> lambda(x, y) where comm(x, y), cuda(block: 32) -> x * y) <&> (method_for(B, B) <@> lambda(x, y) where comm(x, y), cuda(block: 64) -> x * y + 1.0) |> compute
"""
        let softMixed = """
let A = [1.0, 2.0, 3.0, 4.0]
let (u, m2) = (method_for(A) <@> lambda(x) where cuda(block: 64) -> x * 3.0) <&> (method_for(A, A) <@> lambda(x, y) where comm(x, y), cuda(block: 32) -> x * y) |> compute
"""
        // COMPLEX cases (thrust device dialect). The differential demands
        // byte-identical stdout, so these use EXACT complex arithmetic only
        // (mul/add/conj/real/imag on small exact values) — device libm
        // transcendentals differ from the host's by ~1 ulp and get their own
        // structure+run case below instead of a value differential.
        let cxHost = """
let Z = [complex(1.0, 2.0), complex(-0.5, 0.25), complex(3.0, -1.0), complex(0.0, 1.0)]
let R = method_for(Z) <@> lambda(z) -> z * conj(z) + 2.0 * z |> compute
"""
        let cxCuda = """
let Z = [complex(1.0, 2.0), complex(-0.5, 0.25), complex(3.0, -1.0), complex(0.0, 1.0)]
let R = method_for(Z) <@> lambda(z) where cuda(block: 64) -> z * conj(z) + 2.0 * z |> compute
"""
        // MIXED input element types: a complex and a real input in one kernel
        // (rank-2 outer product) — pins the per-position input typing on both
        // sides of the extern "C" boundary.
        let cxMixHost = """
let Z = [complex(1.0, 1.0), complex(2.0, -1.0), complex(-3.0, 0.5)]
let X = [2.0, 3.0, 4.0]
let R = method_for(Z, X) <@> lambda(z, x) -> z * x + conj(z) |> compute
"""
        let cxMixCuda = """
let Z = [complex(1.0, 1.0), complex(2.0, -1.0), complex(-3.0, 0.5)]
let X = [2.0, 3.0, 4.0]
let R = method_for(Z, X) <@> lambda(z, x) where cuda(block: 64) -> z * x + conj(z) |> compute
"""
        // complex() constructor + real/imag accessors in the kernel body: the
        // device dialect renders thrust::complex<double>(...) and the member
        // .real()/.imag() forms (thrust has no free real/imag).
        let cxPartsHost = """
let Z = [complex(1.5, -2.0), complex(0.25, 4.0), complex(-1.0, 3.0)]
let R = method_for(Z) <@> lambda(z) -> complex(imag(z), real(z) * 2.0) |> compute
"""
        let cxPartsCuda = """
let Z = [complex(1.5, -2.0), complex(0.25, 4.0), complex(-1.0, 3.0)]
let R = method_for(Z) <@> lambda(z) where cuda(block: 32) -> complex(imag(z), real(z) * 2.0) |> compute
"""
        // COMPLEX symmetric triangular (simplicial unrank path with a complex
        // source/output): kernel symmetric under swap, exact products.
        let cxSymHost = """
let Z = [complex(1.0, 1.0), complex(2.0, -1.0), complex(0.5, 3.0), complex(-1.0, 2.0)]
let R = method_for(Z, Z) <@> lambda(x, y) where comm(x, y) -> x * y + conj(x) * conj(y) |> compute
"""
        let cxSymCuda = """
let Z = [complex(1.0, 1.0), complex(2.0, -1.0), complex(0.5, 3.0), complex(-1.0, 2.0)]
let R = method_for(Z, Z) <@> lambda(x, y) where comm(x, y), cuda(block: 32) -> x * y + conj(x) * conj(y) |> compute
"""
        // COMPLEX co-fusion: two complex leaves over the same input, one
        // device launch with two complex output buffers.
        let cxCofuseHost = """
let Z = [complex(1.0, 2.0), complex(3.0, -1.0), complex(-2.0, 0.5), complex(0.0, 1.0)]
let (u, v) = (method_for(Z) <@> lambda(z) -> z * 2.0) <&!> (method_for(Z) <@> lambda(z) -> z + conj(z)) |> compute
"""
        let cxCofuseCuda = """
let Z = [complex(1.0, 2.0), complex(3.0, -1.0), complex(-2.0, 0.5), complex(0.0, 1.0)]
let (u, v) = (method_for(Z) <@> lambda(z) where cuda(block: 64) -> z * 2.0) <&!> (method_for(Z) <@> lambda(z) where cuda(block: 64) -> z + conj(z)) |> compute
"""
        // CAPTURED ARRAYS in a MATERIALIZING body. `let c = t * w` is a
        // kernel-local array over a CAPTURED array, so the body has no inline
        // expression form and the S2 router turns it into a call to its lifted
        // host function -- which cannot cross into the .cu. The device form
        // forwards `t`/`s` as device pointers and fuses the map into the
        // prodsum's accumulator loop, so nothing is materialized per thread.
        //
        // Arithmetic is EXACT (small integral/half values: every product and
        // partial sum is representable in binary64), so this is a byte-identical
        // value differential despite FMA contraction differing between the two
        // compilers -- the same reason the complex transcendental case below is
        // structure-only.
        let capHost = """
let t = [1.0, 2.0, 3.0, 4.0]
let s = [0.5, 1.5, 2.5, 3.5]
let ws = [1.0, 2.0, 3.0]
let R = ws <@> lambda(w) -> { let c = t * w
                              prodsum(s, c) } |> compute
"""
        let capCuda = """
let t = [1.0, 2.0, 3.0, 4.0]
let s = [0.5, 1.5, 2.5, 3.5]
let ws = [1.0, 2.0, 3.0]
let R = ws <@> lambda(w) where cuda(block: 32) -> { let c = t * w
                                                   prodsum(s, c) } |> compute
"""
        // REDUCTION JOIN in a device kernel body. Three legs over ONE traversal
        // of the sample axis, two of them reading the same named deferred maps
        // (`c`, `d` -- no `compute`, which is the sharing declaration). On the
        // device that is three per-thread accumulator registers plus a `const`
        // share local per deferred operand; the host oracle is the same loop.
        //
        // Values are exact in binary64 (integers and halves throughout, every
        // partial product and sum representable), so this is a byte-identical
        // differential and not a tolerance comparison -- same rule as
        // `captured_arrays`. A join is a TRAVERSAL rewrite, so anything but
        // equality here means the legs stopped folding the same cells.
        let joinHost = """
let t = [1.0, 2.0, 3.0, 4.0]
let s = [0.5, 1.5, 2.5, 3.5]
let ws = [1.0, 2.0, 3.0]
let R = ws <@> lambda(w) -> {
    let c = t <@> lambda(x) -> x * w
    let d = t <@> lambda(x) -> x + w
    let p, q, r = object_for(<&!>) <@> (prodsum(s, c), prodsum(s, d), prodsum(c, d))
    p + 2.0 * q - r
} |> compute
"""
        let joinCuda = """
let t = [1.0, 2.0, 3.0, 4.0]
let s = [0.5, 1.5, 2.5, 3.5]
let ws = [1.0, 2.0, 3.0]
let R = ws <@> lambda(w) where cuda(block: 32) -> {
    let c = t <@> lambda(x) -> x * w
    let d = t <@> lambda(x) -> x + w
    let p, q, r = object_for(<&!>) <@> (prodsum(s, c), prodsum(s, d), prodsum(c, d))
    p + 2.0 * q - r
} |> compute
"""
        // HETEROGENEOUS legs: three different fold kernels and three different
        // seeds in one device traversal -- `prodsum` folding (+) from 0, `(*)`
        // from 1, and a lambda max from a sentinel. This is what forces per-leg
        // kernel and seed slots on the device side too; a shared accumulator
        // type or a shared operator could not express it at any width.
        let joinHeteroHost = """
let a = [1.0, 2.0, 4.0, 0.5]
let b = [3.0, 1.0, 0.25, 8.0]
let ws = [1.0, 2.0]
let R = ws <@> lambda(w) -> {
    let c = a <@> lambda(x) -> x * w
    let x, y, z = object_for(<&!>) <@> (prodsum(b, c), reduce(c, (*), 1.0), reduce(c, lambda(p, q) -> if p > q then p else q, 0.0 - 1.0e30))
    x + y + z
} |> compute
"""
        let joinHeteroCuda = """
let a = [1.0, 2.0, 4.0, 0.5]
let b = [3.0, 1.0, 0.25, 8.0]
let ws = [1.0, 2.0]
let R = ws <@> lambda(w) where cuda(block: 32) -> {
    let c = a <@> lambda(x) -> x * w
    let x, y, z = object_for(<&!>) <@> (prodsum(b, c), reduce(c, (*), 1.0), reduce(c, lambda(p, q) -> if p > q then p else q, 0.0 - 1.0e30))
    x + y + z
} |> compute
"""
        // A deferred map REUSED after the join that shared it. The share local
        // is scoped to the join's own loop, so the bare `prodsum(c, t)` between
        // the two joins must go back to recomputing `c` -- and the second join
        // must mint its own locals rather than name the first join's. Getting
        // this wrong emits an identifier that is out of scope by the time it is
        // used, which nvcc only reports after the host half has already been
        // told the kernel exists.
        let joinReuseHost = """
let t = [1.0, 2.0, 3.0, 4.0]
let s = [0.5, 1.5, 2.5, 3.5]
let ws = [1.0, 2.0, 3.0]
let R = ws <@> lambda(w) -> {
    let c = t <@> lambda(x) -> x * w
    let d = t <@> lambda(x) -> x + w
    let p, q = object_for(<&!>) <@> (prodsum(s, c), prodsum(c, d))
    let z = prodsum(c, t)
    let e, f = object_for(<&!>) <@> (prodsum(d, d), prodsum(c, s))
    p + q + z + e + f
} |> compute
"""
        let joinReuseCuda = """
let t = [1.0, 2.0, 3.0, 4.0]
let s = [0.5, 1.5, 2.5, 3.5]
let ws = [1.0, 2.0, 3.0]
let R = ws <@> lambda(w) where cuda(block: 32) -> {
    let c = t <@> lambda(x) -> x * w
    let d = t <@> lambda(x) -> x + w
    let p, q = object_for(<&!>) <@> (prodsum(s, c), prodsum(c, d))
    let z = prodsum(c, t)
    let e, f = object_for(<&!>) <@> (prodsum(d, d), prodsum(c, s))
    p + q + z + e + f
} |> compute
"""
        // RUNTIME-EXTENT launch grid: a kernel inside a GENERIC function, whose
        // operand extent is not known until the call. The grid is
        // `(n + block - 1) / block` either way, so the extent travels as a
        // `size_t` parameter instead of being baked as a literal.
        let rtHost = """
function fscale(xs: T^1) -> T^1 = xs <@> lambda(x) -> x * 3.0 + 1.0
let A = [1.0, 2.0, 3.0, 4.0, 5.0]
let R = fscale(A) |> compute
"""
        let rtCuda = """
function fscale(xs: T^1) -> T^1 = xs <@> lambda(x) where cuda(block: 64) -> x * 3.0 + 1.0
let A = [1.0, 2.0, 3.0, 4.0, 5.0]
let R = fscale(A) |> compute
"""
        // Both at once -- a join inside a generic function, which is the shape
        // the whole arc exists for (one per-frequency kernel, k statistics, no
        // extent known before the call). The runtime count reaches three
        // places that have to agree: the thread grid, the staging copies for
        // the forwarded array captures, and the join's own fold bound.
        let rtJoinHost = """
function stats(xs: T^1, ys: T^1, ws: T^1) -> T^1 =
    ws <@> lambda(w) -> {
        let c = xs <@> lambda(x) -> x * w
        let d = xs <@> lambda(x) -> x + w
        let p, q, r = object_for(<&!>) <@> (prodsum(ys, c), prodsum(ys, d), prodsum(c, d))
        p + 2.0 * q - r
    }
let t = [1.0, 2.0, 3.0, 4.0]
let s = [0.5, 1.5, 2.5, 3.5]
let ws = [1.0, 2.0, 3.0]
let R = stats(t, s, ws) |> compute
"""
        let rtJoinCuda = """
function stats(xs: T^1, ys: T^1, ws: T^1) -> T^1 =
    ws <@> lambda(w) where cuda(block: 64) -> {
        let c = xs <@> lambda(x) -> x * w
        let d = xs <@> lambda(x) -> x + w
        let p, q, r = object_for(<&!>) <@> (prodsum(ys, c), prodsum(ys, d), prodsum(c, d))
        p + 2.0 * q - r
    }
let t = [1.0, 2.0, 3.0, 4.0]
let s = [0.5, 1.5, 2.5, 3.5]
let ws = [1.0, 2.0, 3.0]
let R = stats(t, s, ws) |> compute
"""
        let cases =
            [ ("rank1", rank1Host, rank1Cuda)
              ("captured_arrays", capHost, capCuda)
              ("reduction_join", joinHost, joinCuda)
              ("reduction_join_hetero", joinHeteroHost, joinHeteroCuda)
              ("reduction_join_reuse", joinReuseHost, joinReuseCuda)
              ("runtime_extent", rtHost, rtCuda)
              ("runtime_extent_join", rtJoinHost, rtJoinCuda)
              ("complex_rank1", cxHost, cxCuda)
              ("complex_mixed_inputs", cxMixHost, cxMixCuda)
              ("complex_ctor_parts", cxPartsHost, cxPartsCuda)
              ("complex_sym_triangular", cxSymHost, cxSymCuda)
              ("complex_cofuse", cxCofuseHost, cxCofuseCuda)
              ("cofuse_rank1", cofuse1Host, cofuse1Cuda)
              ("cofuse_rank2", cofuse2Host, cofuse2Cuda)
              ("rank2_outer", rank2Host, rank2Cuda)
              ("rank1_multiblock", mbHost, mbCuda)
              ("rank3_nonuniform", rank3Host, rank3Cuda)
              ("int_elem", intHost, intCuda)
              ("poly_body", polyHost, polyCuda)
              ("big_multiblock", bigHost, bigCuda)
              ("sym_triangular", symTriHost, symTriCuda)
              ("anti_triangular", antiTriHost, antiTriCuda)
              ("sym3_simplex", sym3Host, sym3Cuda)
              ("anti3_simplex", anti3Host, anti3Cuda)
              ("sym4_simplex", sym4Host, sym4Cuda)
              ("anti4_simplex", anti4Host, anti4Cuda)
              ("sym5_simplex", sym5Host, sym5Cuda)
              ("anti5_simplex", anti5Host, anti5Cuda)
              ("joint_2d", joint2dHost, joint2dCuda)
              ("softjoin_rect", softRect, softRect)
              ("softjoin_simplicial", softSimp, softSimp)
              ("softjoin_mixed", softMixed, softMixed) ]

        let mutable failures = 0
        let mutable passed = 0
        let mutable failedNames = []
        for (label, hostSrc, cudaSrc) in cases do
            let rc = runCudaCase label hostSrc cudaSrc
            if rc = 0 then passed <- passed + 1
            else (failures <- failures + 1; failedNames <- failedNames @ [label])
        // Host-only: symmetric output exercises the hasRealSymmetry=true branch
        // (named static symm array) under MSVC — the branch the v24 fix did NOT
        // change, so this confirms it still compiles under cl. The PRIMARY signal
        // is that it compiles + runs at all (the symm allocation path is the
        // MSVC-sensitive part); we check only that an R array is printed, NOT a
        // specific value (symmetric print ordering/storage isn't asserted here).
        let symRc = runHostCompileCase "sym_output" symHost "R = ["
        if symRc = 0 then passed <- passed + 1
        else (failures <- failures + 1; failedNames <- failedNames @ ["sym_output"])
        // Soft-join STRUCTURE: the .cu must carry per-leaf kernels + split
        // begin/end wrappers with in-wrapper device selection, and the host
        // must sequence EVERY begin before the FIRST end (that ordering is
        // what multi-device overlap exploits). Call sites are distinguished
        // from the extern-C protos by their pool_base(...) arguments.
        let softStructRc =
            let label = "softjoin_structure"
            try
                CodeGen.setCudaEmitMode true
                let outcome =
                    match genVariant "cuda_softjoin_struct" softRect with
                    | Error e -> Error e
                    | Ok (_, None) -> Error "no .cu emitted for the soft join"
                    | Ok (cppFile, Some cuFile) ->
                        let cu = File.ReadAllText cuFile
                        let cpp = File.ReadAllText cppFile
                        let countOf (hay: string) (needle: string) =
                            let mutable c = 0
                            let mutable i = hay.IndexOf needle
                            while i >= 0 do
                                c <- c + 1
                                i <- hay.IndexOf(needle, i + needle.Length)
                            c
                        let kernels = countOf cu "__global__ void __cuda_"
                        let lastBegin = cpp.LastIndexOf "_begin(pool_base"
                        let firstEnd = cpp.IndexOf "_end(pool_base"
                        if kernels <> 2 then Error ($"expected 2 kernels, .cu has {kernels}")
                        elif not (cu.Contains "cudaGetDeviceCount") then Error "no in-wrapper device query"
                        elif countOf cu "_begin(" < 2 || countOf cu "_end(" < 2 then Error "missing split wrappers"
                        elif lastBegin < 0 || firstEnd < 0 then Error "host begin/end calls not found"
                        elif lastBegin > firstEnd then Error "host does not sequence all begins before ends"
                        else Ok ()
                CodeGen.setCudaEmitMode false
                match outcome with
                | Ok () ->
                    Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Pass label "per-leaf begin/end + device round-robin emitted"
                    0
                | Error e ->
                    Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail label e
                    1
            with ex ->
                CodeGen.setCudaEmitMode false
                Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail label ex.Message
                1
        if softStructRc = 0 then passed <- passed + 1
        else (failures <- failures + 1; failedNames <- failedNames @ ["softjoin_structure"])
        // <&!> stays HARD: the same unfusable pair under mandatory fusion is
        // still a loud codegen diagnostic, not a silent soft-join.
        let hardRc =
            let label = "softjoin_not_for_hard_join"
            let hardSrc = softRect.Replace("<&>", "<&!>")
            try
                CodeGen.setCudaEmitMode true
                let outcome =
                    match genVariant "cuda_softjoin_hard" hardSrc with
                    | Error e -> Error e
                    | Ok (cppFile, _) ->
                        let cpp = File.ReadAllText cppFile
                        if cpp.Contains "cannot fuse" then Ok ()
                        else Error "expected the <&!> cannot-fuse diagnostic in the emitted host code"
                CodeGen.setCudaEmitMode false
                match outcome with
                | Ok () ->
                    Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Pass label "<&!> still rejects loudly"
                    0
                | Error e ->
                    Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail label e
                    1
            with ex ->
                CodeGen.setCudaEmitMode false
                Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail label ex.Message
                1
        if hardRc = 0 then passed <- passed + 1
        else (failures <- failures + 1; failedNames <- failedNames @ ["softjoin_not_for_hard_join"])
        // COMPLEX transcendental on device: exp/conj via thrust. Deliberately
        // NOT a value differential — device libm transcendentals differ from
        // the host's by ~1 ulp, which the byte-identical stdout comparison
        // would flag. Pins STRUCTURE (thrust vocabulary + includes in the
        // .cu) plus that the kernel split-compiles and RUNS on the GPU; value
        // correctness of the complex vocabulary is pinned by the exact-
        // arithmetic differentials above.
        let cxExpRc =
            let label = "complex_exp_device"
            let src = """
let Z = [complex(0.1, 0.2), complex(-0.3, 0.4), complex(0.5, -0.5)]
let R = method_for(Z) <@> lambda(z) where cuda(block: 32) -> exp(z) * conj(z) |> compute
"""
            for ext in [".cu"; ".cpp"; ".cu.obj"; ".cpp.obj"; ".cu.o"; ".cpp.o"; ".exe"; ".out"] do
                let f = Path.Combine(outputDir, "cuda_complex_exp" + ext)
                try if File.Exists f then File.Delete f with _ -> ()
            try
                CodeGen.setCudaEmitMode true
                let outcome =
                    match genVariant "cuda_complex_exp" src with
                    | Error e -> Error e
                    | Ok (_, None) -> Error "no .cu emitted for the complex exp kernel"
                    | Ok (cppFile, Some cuFile) ->
                        let cu = File.ReadAllText cuFile
                        if not (cu.Contains "thrust::exp") then Error ".cu does not use thrust::exp"
                        elif not (cu.Contains "#include <thrust/complex.h>") then Error ".cu missing the thrust include"
                        elif not (cu.Contains "thrust::complex<double>") then Error ".cu missing thrust::complex device buffers"
                        else
                            match compileCudaSplit cuFile cppFile outputDir with
                            | Error e -> Error ($"cuda split-compile: {e}")
                            | Ok exe ->
                                match runExecutable exe with
                                | Error e -> Error ($"run: {e}")
                                | Ok (0, out) when out.Contains "R = [" -> Ok ()
                                | Ok (0, _) -> Error "output missing the R array"
                                | Ok (code, out) -> Error ($"exit {code}:\n{out}")
                CodeGen.setCudaEmitMode false
                match outcome with
                | Ok () ->
                    Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Pass label "thrust transcendental kernel compiles and runs on device"
                    0
                | Error e ->
                    Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail label e
                    1
            with ex ->
                CodeGen.setCudaEmitMode false
                Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail label ex.Message
                1
        if cxExpRc = 0 then passed <- passed + 1
        else (failures <- failures + 1; failedNames <- failedNames @ ["complex_exp_device"])
        // CAPTURE-FORWARDING STRUCTURE. The value differential above proves the
        // kernel computes the right thing; this pins HOW, because the same
        // numbers would come back from a silent host fallback. Three links have
        // to line up or the split build does not resolve: the __global__ takes a
        // device pointer per captured array, the wrapper stages one buffer per
        // capture, and the HOST call site passes that capture's pool. It also
        // pins the fusion -- no per-thread buffer for the intermediate row.
        let capStructRc =
            let label = "capture_forward_structure"
            let src = """
let t = [1.0, 2.0, 3.0, 4.0]
let s = [0.5, 1.5, 2.5, 3.5]
let ws = [1.0, 2.0, 3.0]
let R = ws <@> lambda(w) where cuda(block: 32) -> { let c = t * w
                                                   prodsum(s, c) } |> compute
"""
            for ext in [".cu"; ".cpp"; ".cu.obj"; ".cpp.obj"; ".cu.o"; ".cpp.o"; ".exe"; ".out"] do
                let f = Path.Combine(outputDir, "cuda_capture_struct" + ext)
                try if File.Exists f then File.Delete f with _ -> ()
            try
                CodeGen.setCudaEmitMode true
                let outcome =
                    match genVariant "cuda_capture_struct" src with
                    | Error e -> Error e
                    | Ok (_, None) -> Error "no .cu emitted for the captured-array kernel"
                    | Ok (cppFile, Some cuFile) ->
                        let cu = File.ReadAllText cuFile
                        let cpp = File.ReadAllText cppFile
                        if not (cu.Contains "const double* __blade_cap_t") then
                            Error "the __global__ does not take the captured array as a device pointer"
                        elif not (cu.Contains "cudaMalloc(&__blade_d___blade_cap_t") then
                            Error "the launch wrapper does not stage a device buffer for the capture"
                        elif not (cpp.Contains "pool_base(t.data)") then
                            Error "the host call site does not pass the capture's pool"
                        elif cu.Contains "__lambda_" then
                            Error "the .cu still names a lifted host function"
                        else Ok ()
                CodeGen.setCudaEmitMode false
                match outcome with
                | Ok () ->
                    Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Pass label
                        "captures forwarded as device pointers, staged by the wrapper, passed by the host"
                    0
                | Error e ->
                    Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail label e
                    1
            with ex ->
                CodeGen.setCudaEmitMode false
                Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail label ex.Message
                1
        if capStructRc = 0 then passed <- passed + 1
        else (failures <- failures + 1; failedNames <- failedNames @ ["capture_forward_structure"])
        // REDUCTION JOIN structure. The value differential above cannot tell a
        // fused traversal from four separate ones -- both give the same
        // numbers. So this pins the LOOP: exactly one `for` in the whole .cu,
        // one accumulator per leg, and each shared transcendental evaluated
        // ONCE. Textual for exactly the reason the CPU side's acceptance
        // criterion is textual (docs/plan-reduction-joins.md, section 3 of the
        // differential findings): sharing silently not firing was numerically
        // invisible and only showed up in the emitted code.
        let joinStructRc =
            let label = "reduction_join_structure"
            let src = """
let t = [1.0, 2.0, 3.0, 4.0]
let s = [0.5, 1.5, 2.5, 3.5]
let ws = [1.0, 2.0, 3.0]
let R = ws <@> lambda(w) where cuda(block: 32) -> {
    let ct = t <@> lambda(x) -> cos(w * x)
    let st = t <@> lambda(x) -> sin(w * x)
    let a, b, c, d = object_for(<&!>) <@> (prodsum(s, ct), prodsum(s, st), prodsum(ct, ct), prodsum(ct, st))
    a + b + c + d
} |> compute
"""
            for ext in [".cu"; ".cpp"; ".cu.obj"; ".cpp.obj"; ".cu.o"; ".cpp.o"; ".exe"; ".out"] do
                let f = Path.Combine(outputDir, "cuda_join_struct" + ext)
                try if File.Exists f then File.Delete f with _ -> ()
            let countOf (needle: string) (hay: string) =
                let mutable n, i = 0, 0
                while i >= 0 && i < hay.Length do
                    let j = hay.IndexOf(needle, i)
                    if j < 0 then i <- -1 else (n <- n + 1; i <- j + needle.Length)
                n
            try
                CodeGen.setCudaEmitMode true
                let outcome =
                    match genVariant "cuda_join_struct" src with
                    | Error e -> Error e
                    | Ok (_, None) -> Error "no .cu emitted for the reduction-join kernel"
                    | Ok (_cppFile, Some cuFile) ->
                        let cu = File.ReadAllText cuFile
                        let loops = countOf "for (" cu
                        let accs = countOf "_jacc" cu
                        if loops <> 1 then
                            Error ($"the join emitted {loops} loops, not ONE fused traversal")
                        // 4 declarations + 4 reads on the left of the fold + 4
                        // on the right + 4 tail reads = the accumulators are
                        // used, not merely declared. The exact count is not the
                        // point; ZERO would mean the join never reached the
                        // device emitter at all.
                        elif accs < 4 then
                            Error ($"the join declared {accs} accumulator references (expected one per leg)")
                        elif countOf "cos(" cu <> 1 then
                            Error ($"""the shared `cos` is evaluated {(countOf "cos(" cu)} times per iteration, not once""")
                        elif countOf "sin(" cu <> 1 then
                            Error ($"""the shared `sin` is evaluated {(countOf "sin(" cu)} times per iteration, not once""")
                        elif not (cu.Contains "const double") then
                            Error "the shared deferred operands are not per-thread const locals"
                        else Ok ()
                CodeGen.setCudaEmitMode false
                match outcome with
                | Ok () ->
                    Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Pass label
                        "one fused loop, one accumulator per leg, each shared map evaluated once"
                    0
                | Error e -> Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail label e; 1
            with ex ->
                CodeGen.setCudaEmitMode false
                Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail label ex.Message
                1
        if joinStructRc = 0 then passed <- passed + 1
        else (failures <- failures + 1; failedNames <- failedNames @ ["reduction_join_structure"])
        // RUNTIME-EXTENT structure. Four things have to line up or the launch
        // is sized by something other than the actual operand: the __global__
        // takes the extent as a parameter and unranks against it, the wrapper
        // derives the grid from it, the staging copies use it, and the HOST
        // reads it off the operand's own extents table. Pinned because a
        // literal that happened to match would pass the value differential.
        let rtStructRc =
            let label = "runtime_extent_structure"
            let src = """
function fscale(xs: T^1) -> T^1 = xs <@> lambda(x) where cuda(block: 64) -> x * 3.0 + 1.0
let A = [1.0, 2.0, 3.0, 4.0, 5.0]
let R = fscale(A) |> compute
"""
            for ext in [".cu"; ".cpp"; ".cu.obj"; ".cpp.obj"; ".cu.o"; ".cpp.o"; ".exe"; ".out"] do
                let f = Path.Combine(outputDir, "cuda_rtext_struct" + ext)
                try if File.Exists f then File.Delete f with _ -> ()
            try
                CodeGen.setCudaEmitMode true
                let outcome =
                    match genVariant "cuda_rtext_struct" src with
                    | Error e -> Error e
                    | Ok (_, None) -> Error "no .cu emitted for the runtime-extent kernel"
                    | Ok (cppFile, Some cuFile) ->
                        let cu = File.ReadAllText cuFile
                        let cpp = File.ReadAllText cppFile
                        if not (cu.Contains "size_t __blade_ext0") then
                            Error "the kernel does not take the runtime extent as a parameter"
                        elif not (cu.Contains "__blade_card = __blade_ext0") then
                            Error "the launch grid is not derived from the runtime extent"
                        elif not (cu.Contains "__blade_ext0 * sizeof") then
                            Error "the staging copy is not sized by the runtime extent"
                        elif not (cpp.Contains "xs.extents[0]") then
                            Error "the host call site does not read the extent off the operand"
                        else Ok ()
                CodeGen.setCudaEmitMode false
                match outcome with
                | Ok () ->
                    Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Pass label
                        "grid, staging and unranking all derived from the runtime extent"
                    0
                | Error e -> Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail label e; 1
            with ex ->
                CodeGen.setCudaEmitMode false
                Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail label ex.Message
                1
        if rtStructRc = 0 then passed <- passed + 1
        else (failures <- failures + 1; failedNames <- failedNames @ ["runtime_extent_structure"])
        printFooter "CUDA Kernel" [$"{passed} passed"; $"{failures} failure(s)"]
        { Block = "CUDA Kernel"; Passed = passed; Failed = failures; Skipped = 0; FailedNames = failedNames }


/// Run `cpp/cublas_swap_tests.cu` — the runtime verification of Round D's
/// COLUMN-MAJOR SWAP TABLE (docs/plan-cpp-perf-exploitation.md).
///
/// WHY IT LIVES HERE AND NOT IN `blade test linalg`. Its sibling
/// `runLinAlgProbeTests` needs only g++ and runs in the default suite; this one
/// needs nvcc, an MSVC host compiler on Windows, and a real GPU — the exact
/// capability set this file already gates on. Putting it under the opt-in
/// `--cuda` phase keeps the default suite free of a ~30 s nvcc compile and of a
/// hardware dependency, and puts it beside the other tests that would skip for
/// the same reasons.
///
/// WHAT IT PROVES THAT EMISSION TESTS CANNOT. `blade test linalg` shows that a
/// gram/matmul REACHES `blade_cuda_*`. Whether the column-major swap inside
/// those entry points computes the right matrix is invisible in emitted text
/// and nearly invisible in values — a wrong transpose flag, a missing
/// conjugation or an unflipped fill mode all return a plausible matrix rather
/// than an error. The probe therefore computes each (route x precision) twice,
/// once on the device and once through a host loop transcribing Blade's own
/// arithmetic, and compares. Tolerance, not byte-identity: cuBLAS accumulates
/// in a different order, while a swap error is thousands of ULPs out.
///
/// Skips cleanly (Skipped = 1) when the toolchain or the GPU is absent; never
/// fails for environment reasons.
let runCublasSwapTests () : Blade.Tests.TestHarness.BlockResult =
    let blockName = "cuBLAS Swap"
    printHeader "cuBLAS Swap-Table Verification"
    let skipResult = { Block = blockName; Passed = 0; Failed = 0; Skipped = 1; FailedNames = [] }
    let caps = capabilities.Value
    let onWindows = RuntimeInformation.IsOSPlatform(OSPlatform.Windows)
    let cppDir = Path.Combine(AppContext.BaseDirectory, "cpp")
    let testSrc = Path.Combine(cppDir, "cublas_swap_tests.cu")
    if not caps.HasNvcc || not caps.HasGpu then
        printfn "Skipped: requires nvcc + CUDA GPU (nvcc=%b, gpu=%b)." caps.HasNvcc caps.HasGpu
        skipResult
    elif onWindows && not caps.HasCl then
        printfn "Skipped: nvcc needs cl.exe (MSVC) as host compiler, not found on PATH."
        printfn "         Run from the 'x64 Native Tools Command Prompt for VS' (or after vcvars64.bat)."
        skipResult
    elif not (File.Exists testSrc) then
        eprintfn "cublas_swap_tests.cu not found at: %s" testSrc
        eprintfn "Check that Blade.fsproj copies cpp/cublas_swap_tests.cu to the output dir."
        { Block = blockName; Passed = 0; Failed = 1; Skipped = 0
          FailedNames = ["cublas_swap_tests.cu missing"] }
    else
        let exeExt = if onWindows then ".exe" else ".out"
        let exePath = Path.ChangeExtension(testSrc, exeExt)
        // Compiled IN cppDir so `#include "blade_linalg_cuda.hpp"` (and the
        // views header it pulls in) resolve to the SHIPPED headers the codegen
        // path deploys — testing a stale copy would defeat the point. nvcc stays
        // at -O2, the rule for every nvcc path in this repo.
        let args =
            if onWindows then
                $"-std=c++17 -O2 -Xcompiler /Zc:preprocessor -o \"{exePath}\" \"{testSrc}\" -lcublas"
            else
                $"-std=c++17 -O2 -o \"{exePath}\" \"{testSrc}\" -lcublas"
        let runIn (exe: string) (a: string) (timeoutMs: int) =
            let psi = ProcessStartInfo(exe, a)
            psi.RedirectStandardOutput <- true
            psi.RedirectStandardError <- true
            psi.UseShellExecute <- false
            psi.CreateNoWindow <- true
            psi.WorkingDirectory <- cppDir
            use p = Process.Start(psi)
            let o = Blade.Runtime.readToEndOffPool p.StandardOutput
            let e = Blade.Runtime.readToEndOffPool p.StandardError
            let exited = p.WaitForExit(timeoutMs)
            if not exited then (try p.Kill(true) with _ -> ())
            (exited, (if exited then p.ExitCode else -1), o.Result, e.Result)
        let (cExited, cCode, cOut, cErr) = runIn "nvcc" args 300000
        if not cExited then
            printfn "nvcc compilation TIMED OUT (300s)"
            printFooter blockName ["FAILED"]
            { Block = blockName; Passed = 0; Failed = 1; Skipped = 0; FailedNames = ["<compile timeout>"] }
        elif cCode <> 0 then
            printfn "nvcc compilation FAILED:"
            printfn "%s" (cOut + "\n" + cErr)
            printFooter blockName ["FAILED"]
            { Block = blockName; Passed = 0; Failed = 1; Skipped = 0; FailedNames = ["<compile failed>"] }
        else
            let (rExited, rCode, rOut, rErr) = runIn exePath "" 120000
            printf "%s" rOut
            if not (String.IsNullOrWhiteSpace rErr) then eprintf "%s" rErr
            let outText = rOut.Replace("\r\n", "\n")
            let m =
                System.Text.RegularExpressions.Regex.Match(
                    outText, @"CUBLAS SWAP TESTS:\s*(\d+)/(\d+)\s*passed")
            let pPassed = if m.Success then int m.Groups.[1].Value else 0
            let pTotal = if m.Success then int m.Groups.[2].Value else 0
            let failNames =
                outText.Split('\n')
                |> Array.choose (fun l ->
                    let fm = System.Text.RegularExpressions.Regex.Match(l, @"\[FAIL\]:\s*(.+)$")
                    if fm.Success then Some (fm.Groups.[1].Value.Trim()) else None)
                |> Array.toList
            // Same doctrine as the linalg probe: the summary line must be
            // present before an exit 0 counts as a pass, so a binary that
            // aborted inside the shim's own error path (which calls abort())
            // cannot score a vacuous 0/0.
            if not rExited then
                printFooter blockName ["FAILED"]
                { Block = blockName; Passed = 0; Failed = 1; Skipped = 0; FailedNames = ["<run timeout>"] }
            elif not m.Success then
                printFooter blockName ["FAILED"]
                printfn "  no 'CUBLAS SWAP TESTS: p/n passed' summary in output -- cannot confirm any check ran"
                { Block = blockName; Passed = 0; Failed = 1; Skipped = 0
                  FailedNames = ["<no CUBLAS SWAP TESTS summary line>"] }
            elif rCode = 0 then
                printFooter blockName ["all passed"]
                { Block = blockName; Passed = pPassed; Failed = 0; Skipped = 0; FailedNames = [] }
            else
                printFooter blockName ["FAILED"]
                { Block = blockName; Passed = pPassed; Failed = max 1 (pTotal - pPassed); Skipped = 0
                  FailedNames = (if failNames.IsEmpty then [$"<exit {rCode}>"] else failNames) }
