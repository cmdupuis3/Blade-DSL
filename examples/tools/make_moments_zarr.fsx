// make_moments_zarr.fsx — deterministic fixture generator for the
// matched-moment discriminator notebook (examples/matched_moments.bladenb).
//
// Four classes of 3-D point clouds, engineered so that the K >= 1 jet blocks
// are the ONLY signal, by construction:
//
//   0 iso    Gaussian                      — the null
//   1 octa   6 lobes along +-x, +-y, +-z   — K4 l=4 (even anisotropy)
//   2 tetra  4 lobes at tetrahedral verts  — K3 (parity-odd anisotropy)
//   3 heavy  radial scale-mixture Gaussian — K4 l=0 (kurtosis)
//
// Two exactness devices, applied HERE, in this order (documented because they
// interact):
//
//   1. RADIAL QUANTILE MATCHING (iso/octa/tetra only; heavy IS the radial
//      class and skips it). Each sample's radii are monotonically re-mapped
//      onto ONE fixed reference profile r*(k) — the elementwise average of
//      the iso class's whitened sorted radii. The map scales each point along
//      its own direction, so it is equivariant and anisotropy-preserving.
//      After it, iso/octa/tetra have IDENTICAL radial quantiles, so every
//      radial functional — shell occupancies (K=0), mean |v|^2, mean |v|^4 —
//      is identical across the matched three.
//
//   2. PER-SAMPLE WHITENING, ALWAYS LAST. Center, then apply the sample's
//      own symmetric inverse square root Sigma^(-1/2) (3x3 Jacobi
//      eigendecomposition, plain F#). Equivariant (W -> R W R^T under
//      rotation of the cloud). After it, K1 = 0 and sample covariance = I to
//      fp rounding — identical as BYTES, not matched in distribution. This
//      is what makes the notebook's K<=2 theorem exact at every N: sampling
//      fluctuations of a covariance estimator carry fourth-moment
//      information, so only exact per-sample whitening closes that leak.
//
//   The two steps fight: re-mapping radii perturbs the covariance, and
//   whitening perturbs the radii. So the matched classes ITERATE
//   [quantile-match -> center -> whiten] to joint convergence (tolerance
//   1e-13 on the post-whiten radial residual, cap 400 iterations), keeping
//   whitening as the final step of every pass — the K<=2 pins are exact by
//   construction, the radial pins carry the (printed) converged residual.
//   The per-sample Haar rotation is applied AFTER convergence, followed by
//   one final center+whiten polish (a rotation is orthogonal, so the polish
//   matrix is I to ~1e-15 and the radial residual survives; both residuals
//   are printed below and asserted).
//
// Also baked: a 12-lobe ICOSAHEDRAL demo cloud, processed exactly like a
// matched class member. Icosahedral order first appears at l = 6, so every
// jet this compiler can build (K <= 4, BL5000) sees it as isotropic — the
// notebook's K<=4-boundary cell pins that.
//
// Run from anywhere:  dotnet fsi examples/tools/make_moments_zarr.fsx
// Idempotent: no RNG state crosses a run boundary; a second run rewrites
// identical bytes (the store content hash is printed; two runs must agree).

#r "System.Security.Cryptography"
// The prefix of Blade.fsproj's compile order that IR.fs and ZarrProvider need,
// in that order — fsi has no project file, so the dependency chain is spelled
// out here, exactly as make_tetris_zarr.fsx does.
#load "../../src/Runtime.fs"
#load "../../src/Platforms.fs"
#load "../../src/Ast.fs"
#load "../../src/Diagnostics.fs"
#load "../../src/Types.fs"
#load "../../src/PerfCounters.fs"
#load "../../src/SimplexBlocksCore.fs"
#load "../../src/OrbRank.fs"
#load "../../src/IR.fs"
#load "../../src/IRLoopStructure.fs"
#load "../../src/IRStorage.fs"
#load "../../src/IRLift.fs"
#load "../../src/IRMono.fs"
#load "../../src/IRPrint.fs"
#load "../../src/IRValidate.fs"
#load "../../src/providers/ProviderRegistry.fs"
#load "../../src/providers/ZarrProvider.fs"
// The rotation sampler, from the BladeML oracle project.
#load "../../src/ml/compiler/MLSpec.fs"
#load "../../oracles/ml/MathUtils.fs"
#load "../../oracles/ml/Irreps.fs"
#load "../../oracles/ml/IrrepsIdx.fs"
#load "../../oracles/ml/Wigner.fs"
#load "../../oracles/ml/SphericalHarmonics.fs"
#load "../../oracles/ml/Rotations.fs"

open System
open System.IO
open System.Security.Cryptography
open Blade.ZarrProvider
open BladeML

// ---------------------------------------------------------------------------
// Sizes and seeds. Non-power-of-two where a fold walks them.
// ---------------------------------------------------------------------------
let nPoint = 120            // points per cloud
let nTrainPer = 100         // per class
let nTestPer = 50
let nClass = 4
let nTrain = nClass * nTrainPer     // 400, class-major
let nTest = nClass * nTestPer       // 200, class-major
let nEpochPerm = 2          // shuffled-label nulls for the diagnostic
let nRot = 8                // fresh verification rotations

let seedGenBase = 20260827  // + class index -> per-class generator stream
let seedRotTrain = 31415926
let seedRotTest = 27182818
let seedRotVerify = 8272026
let seedPerm = 57721566
let seedIcosa = 16180339

// Class knobs (see the printed separation diagnostics; chosen so the
// per-sample z-separation of each class's OWN block is comfortable — roughly
// 5-15 sigma — while everything below that block is matched exactly).
let octaC, octaTau = 1.0, 0.32      // lobe radius, lobe width
let tetraC, tetraTau = 1.0, 0.38
let heavyPHi, heavySHi = 0.1667, 4.5   // scale-mixture: 1 point in 6 at scale 4.5

// ---------------------------------------------------------------------------
// Small numerics: Gaussian draws, 3x3 Jacobi symmetric eigendecomposition,
// symmetric inverse square root, whitening, radial quantile matching.
// ---------------------------------------------------------------------------
/// Box-Muller pair, consuming exactly two uniforms per call pair.
let gaussPair (rng: Random) =
    let u1 = 1.0 - rng.NextDouble()   // avoid log 0
    let u2 = rng.NextDouble()
    let r = sqrt (-2.0 * log u1)
    r * cos (2.0 * Math.PI * u2), r * sin (2.0 * Math.PI * u2)

/// One standard 3-vector: three gaussians (one pair + half of a second).
let gauss3 (rng: Random) =
    let a, b = gaussPair rng
    let c, _ = gaussPair rng
    [| a; b; c |]

/// Jacobi eigendecomposition of a symmetric 3x3: returns (eigenvalues,
/// eigenvectors-as-columns). Deterministic sweep order, tolerance 1e-15.
let jacobiEig (m0: float[,]) =
    let a = Array2D.copy m0
    let v = Array2D.init 3 3 (fun i j -> if i = j then 1.0 else 0.0)
    let off () =
        abs a.[0,1] + abs a.[0,2] + abs a.[1,2]
    let mutable sweep = 0
    while off () > 1e-15 && sweep < 100 do
        sweep <- sweep + 1
        for p in 0 .. 1 do
            for q in p + 1 .. 2 do
                if abs a.[p,q] > 1e-18 then
                    let theta = (a.[q,q] - a.[p,p]) / (2.0 * a.[p,q])
                    let t =
                        let s = if theta >= 0.0 then 1.0 else -1.0
                        s / (abs theta + sqrt (theta * theta + 1.0))
                    let c = 1.0 / sqrt (t * t + 1.0)
                    let s = t * c
                    for k in 0 .. 2 do
                        let akp = a.[k,p]
                        let akq = a.[k,q]
                        a.[k,p] <- c * akp - s * akq
                        a.[k,q] <- s * akp + c * akq
                    for k in 0 .. 2 do
                        let apk = a.[p,k]
                        let aqk = a.[q,k]
                        a.[p,k] <- c * apk - s * aqk
                        a.[q,k] <- s * apk + c * aqk
                    for k in 0 .. 2 do
                        let vkp = v.[k,p]
                        let vkq = v.[k,q]
                        v.[k,p] <- c * vkp - s * vkq
                        v.[k,q] <- s * vkp + c * vkq
    [| a.[0,0]; a.[1,1]; a.[2,2] |], v

/// Symmetric inverse square root of a symmetric positive-definite 3x3.
let invSqrtSym (m: float[,]) =
    let lam, v = jacobiEig m
    let w = Array2D.init 3 3 (fun i j ->
        let mutable s = 0.0
        for k in 0 .. 2 do
            if lam.[k] <= 0.0 then failwithf "covariance not PD (lambda=%g)" lam.[k]
            s <- s + v.[i,k] * v.[j,k] / sqrt lam.[k]
        s)
    // Symmetrize against rounding: the SYMMETRIC root is the equivariant one.
    Array2D.init 3 3 (fun i j -> 0.5 * (w.[i,j] + w.[j,i]))

/// Center a cloud (in place semantics: returns a new array) and whiten with
/// its own Sigma^(-1/2), Sigma = (1/N) sum v v^T. K1 = 0 and cov = I after.
let centerWhiten (pts: float[][]) =
    let n = pts.Length
    let mean = Array.init 3 (fun c -> (pts |> Array.sumBy (fun p -> p.[c])) / float n)
    let ctr = pts |> Array.map (fun p -> Array.init 3 (fun c -> p.[c] - mean.[c]))
    let cov = Array2D.init 3 3 (fun i j ->
        (ctr |> Array.sumBy (fun p -> p.[i] * p.[j])) / float n)
    let w = invSqrtSym cov
    ctr |> Array.map (fun p ->
        Array.init 3 (fun i -> w.[i,0] * p.[0] + w.[i,1] * p.[1] + w.[i,2] * p.[2]))

let norm3 (p: float[]) = sqrt (p.[0] * p.[0] + p.[1] * p.[1] + p.[2] * p.[2])

/// Monotone radial re-map onto the reference profile: the point of radial
/// rank k is scaled along its own direction to radius refR.[k].
let qmatch (refR: float[]) (pts: float[][]) =
    let n = pts.Length
    let order = Array.init n id |> Array.sortBy (fun i -> norm3 pts.[i])
    let out = Array.zeroCreate<float[]> n
    for k in 0 .. n - 1 do
        let i = order.[k]
        let r = norm3 pts.[i]
        let s = refR.[k] / r
        out.[i] <- Array.init 3 (fun c -> pts.[i].[c] * s)
    out

let sortedRadii (pts: float[][]) = pts |> Array.map norm3 |> Array.sort

/// Post-whiten radial residual against the reference profile.
let radialResidual (refR: float[]) (pts: float[][]) =
    let r = sortedRadii pts
    Array.init pts.Length (fun k -> abs (r.[k] - refR.[k])) |> Array.max

/// K1 / K2 exactness residuals of a cloud (max abs mean component, max abs
/// cov-minus-identity entry).
let whitenResidual (pts: float[][]) =
    let n = pts.Length
    let mean = Array.init 3 (fun c -> (pts |> Array.sumBy (fun p -> p.[c])) / float n)
    let covR =
        [| for i in 0 .. 2 do
             for j in 0 .. 2 ->
               let s = (pts |> Array.sumBy (fun p -> (p.[i] - mean.[i]) * (p.[j] - mean.[j]))) / float n
               abs (s - (if i = j then 1.0 else 0.0)) |]
        |> Array.max
    (mean |> Array.map abs |> Array.max), covR

// ---------------------------------------------------------------------------
// Raw generators. Lobe assignment is round-robin (point k -> lobe k mod L):
// exactly balanced occupancy, so the class signal is in the lobe geometry,
// not in multinomial count noise. All randomness is the per-point offsets.
// ---------------------------------------------------------------------------
let octaVerts =
    [| [| 1.0; 0.0; 0.0 |]; [| -1.0; 0.0; 0.0 |]
       [| 0.0; 1.0; 0.0 |]; [| 0.0; -1.0; 0.0 |]
       [| 0.0; 0.0; 1.0 |]; [| 0.0; 0.0; -1.0 |] |]
let tetraVerts =
    let s = 1.0 / sqrt 3.0
    [| [| s; s; s |]; [| s; -s; -s |]; [| -s; s; -s |]; [| -s; -s; s |] |]
/// The 12 icosahedron vertices (cyclic (0, +-1, +-phi)), unit-normalized.
let icosaVerts =
    let phi = (1.0 + sqrt 5.0) / 2.0
    let nrm = sqrt (1.0 + phi * phi)
    [| for perm in 0 .. 2 do
         for s1 in [ 1.0; -1.0 ] do
           for s2 in [ 1.0; -1.0 ] do
             let v = [| 0.0; s1 / nrm; s2 * phi / nrm |]
             yield Array.init 3 (fun i -> v.[(i + 3 - perm) % 3]) |]

let genLobe (verts: float[][]) (c: float) (tau: float) (rng: Random) =
    Array.init nPoint (fun k ->
        let v = verts.[k % verts.Length]
        let g = gauss3 rng
        Array.init 3 (fun i -> c * v.[i] + tau * g.[i]))

let genIso (rng: Random) = Array.init nPoint (fun _ -> gauss3 rng)

let genHeavy (rng: Random) =
    // The heavy assignment is round-robin too (point k heavy iff k mod 5 = 0):
    // exactly nPoint/5 heavy points per sample, so the class signal is the
    // radial LAW, not binomial count noise in how many heavy points landed.
    Array.init nPoint (fun k ->
        let s = if k % 6 = 0 then heavySHi else 1.0
        let g = gauss3 rng
        Array.init 3 (fun i -> s * g.[i]))

let genClass (cls: int) (rng: Random) =
    match cls with
    | 0 -> genIso rng
    | 1 -> genLobe octaVerts octaC octaTau rng
    | 2 -> genLobe tetraVerts tetraC tetraTau rng
    | _ -> genHeavy rng

// One rng per class, 150 clouds each: first 100 train, last 50 test.
let rawClouds =
    Array.init nClass (fun cls ->
        let rng = Random(seedGenBase + cls)
        Array.init (nTrainPer + nTestPer) (fun _ -> genClass cls rng))

// ---------------------------------------------------------------------------
// The reference radial profile r*: elementwise average of the iso class's
// whitened sorted radii — one fixed vector, used for every matched sample.
// ---------------------------------------------------------------------------
let refRadii =
    let acc = Array.zeroCreate<float> nPoint
    let mutable cnt = 0
    for cloud in rawClouds.[0] do
        let r = sortedRadii (centerWhiten cloud)
        for k in 0 .. nPoint - 1 do acc.[k] <- acc.[k] + r.[k]
        cnt <- cnt + 1
    let avg = acc |> Array.map (fun s -> s / float cnt)
    // CONSISTENCY NORMALIZATION: whitening forces sum r^2 = 3N exactly (trace
    // of the identity covariance), while an average of sorted-radius vectors
    // has sum r*^2 < 3N by Jensen. Without this rescale the two constraint
    // sets have no intersection and the joint iteration plateaus at the trace
    // gap (~3e-3, measured) instead of converging.
    let s = sqrt (3.0 * float nPoint / (avg |> Array.sumBy (fun x -> x * x)))
    avg |> Array.map (fun x -> s * x)

/// Iterate [qmatch -> center -> whiten] to joint convergence; whitening is
/// the LAST step of every pass, so K1/K2 exactness never depends on the
/// convergence tolerance — only the radial residual does.
let processMatched (cloud: float[][]) =
    let mutable pts = centerWhiten cloud
    let mutable it = 0
    let mutable res = radialResidual refRadii pts
    while res > 1e-13 && it < 400 do
        pts <- centerWhiten (qmatch refRadii pts)
        res <- radialResidual refRadii pts
        it <- it + 1
    pts, res, it

let processHeavy (cloud: float[][]) =
    // Whiten twice: the second pass polishes the first's rounding to ~1e-16.
    centerWhiten (centerWhiten cloud), 0.0, 0

// Processed, unrotated clouds (the diagnostics below read these — every
// statistic of interest is rotation-invariant, so the pre-rotation frame,
// which is the lobe frame, is the honest place to measure).
let processed =
    Array.init nClass (fun cls ->
        rawClouds.[cls] |> Array.map (fun c ->
            if cls = 3 then processHeavy c else processMatched c))

let matchIterStats =
    let its =
        [| for cls in 0 .. 2 do
             for (_, _, it) in processed.[cls] -> it |]
    Array.min its, Array.max its,
    (its |> Array.sumBy float) / float its.Length
let matchResidualMax =
    [| for cls in 0 .. 2 do
         for (_, res, _) in processed.[cls] -> res |]
    |> Array.max
if matchResidualMax > 1e-10 then
    failwithf "radial quantile matching did not converge (residual %g)" matchResidualMax

// ---------------------------------------------------------------------------
// Separation diagnostics, per class, on the processed unrotated clouds.
//   a4  = mean (x^4+y^4+z^4)/r^4      cubic l=4 direction statistic (iso: 0.6)
//   a3  = mean xyz/r^3                tetrahedral l=3 direction statistic
//   r4  = mean r^4                    the K4 l=0 radial moment
// The matched three have IDENTICAL r4 (same radii); heavy differs in its mean;
// octa moves a4, tetra moves a3. z = gap over rms spread: the "comfortable but
// not trivial" knob check.
// ---------------------------------------------------------------------------
let stat (f: float[] -> float) (pts: float[][]) =
    (pts |> Array.sumBy f) / float pts.Length
let a4 = stat (fun p ->
    let r2 = p.[0]*p.[0] + p.[1]*p.[1] + p.[2]*p.[2]
    (p.[0]**4.0 + p.[1]**4.0 + p.[2]**4.0) / (r2 * r2))
let a3 = stat (fun p ->
    let r = norm3 p
    p.[0] * p.[1] * p.[2] / (r * r * r))
let r4 = stat (fun p ->
    let r2 = p.[0]*p.[0] + p.[1]*p.[1] + p.[2]*p.[2]
    r2 * r2)

let meanSd (xs: float[]) =
    let mu = Array.average xs
    mu, sqrt ((xs |> Array.sumBy (fun x -> (x - mu) * (x - mu))) / float xs.Length)
let classStat f cls = processed.[cls] |> Array.map (fun (p, _, _) -> f p) |> meanSd
let zsep (muA, sdA) (muB, sdB) = abs (muA - muB) / sqrt (sdA * sdA + sdB * sdB + 1e-300)

// ---------------------------------------------------------------------------
// Rotate every sample by its own fresh Haar rotation, then one final
// center+whiten polish (the rotation is orthogonal, so the polish is ~I and
// the radial residual survives — re-checked below).
// ---------------------------------------------------------------------------
let applyRot (r: float[][]) (pts: float[][]) =
    pts |> Array.map (fun p ->
        Array.init 3 (fun i -> r.[i].[0] * p.[0] + r.[i].[1] * p.[1] + r.[i].[2] * p.[2]))

let finalize (rng: Random) (pts: float[][]) =
    centerWhiten (applyRot (Rotations.randomRotation rng) pts)

let rngRotTrain = Random(seedRotTrain)
let rngRotTest = Random(seedRotTest)
// Order of rotation draws: class-major train, then class-major test — fixed.
let finalTrain =
    Array.init nClass (fun cls ->
        Array.init nTrainPer (fun s ->
            let (p, _, _) = processed.[cls].[s]
            finalize rngRotTrain p))
let finalTest =
    Array.init nClass (fun cls ->
        Array.init nTestPer (fun s ->
            let (p, _, _) = processed.[cls].[nTrainPer + s]
            finalize rngRotTest p))

// The icosahedral demo cloud: 12 lobes, matched + whitened + rotated like any
// matched-class member. Its own seeds; not part of any split.
let demoIcosa =
    let rng = Random(seedIcosa)
    let raw = genLobe icosaVerts 1.0 0.45 rng
    let (p, res, it) = processMatched raw
    printfn "  [icosa demo: qmatch %d iters, residual %g]" it res
    finalize (Random(seedIcosa + 1)) p

// Final exactness residuals over every stored sample (post rotation+polish).
let allFinal =
    [| for cls in 0 .. nClass - 1 do
         yield! finalTrain.[cls]
         yield! finalTest.[cls]
       yield demoIcosa |]
let k1ResMax = allFinal |> Array.map (whitenResidual >> fst) |> Array.max
let k2ResMax = allFinal |> Array.map (whitenResidual >> snd) |> Array.max
if k1ResMax > 1e-13 || k2ResMax > 1e-12 then
    failwithf "whitening not exact (K1 %g, K2 %g)" k1ResMax k2ResMax
let finalRadialResMax =
    [| for cls in 0 .. 2 do
         for s in 0 .. nTrainPer - 1 -> radialResidual refRadii finalTrain.[cls].[s]
       for cls in 0 .. 2 do
         for s in 0 .. nTestPer - 1 -> radialResidual refRadii finalTest.[cls].[s]
       yield radialResidual refRadii demoIcosa |]
    |> Array.max
if finalRadialResMax > 1e-10 then
    failwithf "radial match did not survive rotation+polish (%g)" finalRadialResMax

// ---------------------------------------------------------------------------
// Shell occupancies (the K=0 features the notebook's first rung reads):
// 5 Gaussian shells at the 10/30/50/70/90th percentile radii of r*, width
// half the mean spacing. Matched samples must agree to fp; heavy must not.
// ---------------------------------------------------------------------------
let shellMu = [| refRadii.[11]; refRadii.[35]; refRadii.[59]; refRadii.[83]; refRadii.[107] |]
let shellSg =
    let spacing = (shellMu.[4] - shellMu.[0]) / 4.0
    Array.create 5 (0.5 * spacing)
let occupancy (pts: float[][]) =
    Array.init 5 (fun s ->
        (pts |> Array.sumBy (fun p ->
            let d = (norm3 p - shellMu.[s]) / shellSg.[s]
            exp (-0.5 * d * d))) / float nPoint)
let occRef = occupancy finalTrain.[0].[0]
let occDevMatched =
    [| for cls in 0 .. 2 do
         for s in 0 .. nTrainPer - 1 do
           let o = occupancy finalTrain.[cls].[s]
           for k in 0 .. 4 -> abs (o.[k] - occRef.[k]) |]
    |> Array.max
let occDevHeavy =
    [| for s in 0 .. nTrainPer - 1 do
         let o = occupancy finalTrain.[3].[s]
         for k in 0 .. 4 -> abs (o.[k] - occRef.[k]) |]
    |> Array.max
if occDevMatched > 1e-10 then failwithf "matched occupancies differ (%g)" occDevMatched
if occDevHeavy < 1e-3 then failwithf "heavy occupancies do not differ (%g)" occDevHeavy

// ---------------------------------------------------------------------------
// Flatten to store layout: class-major (sample, point, xyz), plus the
// (y, z, x) copy the l=1 irreps basis wants; labels; one-hots; the shuffled-
// label permutations; the fresh verification rotations.
// ---------------------------------------------------------------------------
let flatten (byClass: float[][][][]) nPer =
    let pos = Array.zeroCreate<float> (nClass * nPer * nPoint * 3)
    let posYzx = Array.zeroCreate<float> (nClass * nPer * nPoint * 3)
    let lab = Array.zeroCreate<int64> (nClass * nPer)
    for cls in 0 .. nClass - 1 do
        for s in 0 .. nPer - 1 do
            let smp = cls * nPer + s
            lab.[smp] <- int64 cls
            for p in 0 .. nPoint - 1 do
                let v = byClass.[cls].[s].[p]
                let b = (smp * nPoint + p) * 3
                pos.[b] <- v.[0]; pos.[b + 1] <- v.[1]; pos.[b + 2] <- v.[2]
                posYzx.[b] <- v.[1]; posYzx.[b + 1] <- v.[2]; posYzx.[b + 2] <- v.[0]
    pos, posYzx, lab

let posTrain, posTrainYzx, labTrain = flatten finalTrain nTrainPer
let posTest, posTestYzx, labTest = flatten finalTest nTestPer

let oneHot (lab: int64[]) =
    let y = Array.zeroCreate<float> (lab.Length * nClass)
    lab |> Array.iteri (fun i l -> y.[i * nClass + int l] <- 1.0)
    y
let yTrain = oneHot labTrain
let yTest = oneHot labTest

let demoFlat = [| for p in demoIcosa do yield! p |]
let demoFlatYzx = [| for p in demoIcosa do yield p.[1]; yield p.[2]; yield p.[0] |]

// Fisher-Yates permutations of the train axis, for the shuffled-label null.
let perm =
    let rng = Random(seedPerm)
    let p = Array.zeroCreate<int64> (nEpochPerm * nTrain)
    for ep in 0 .. nEpochPerm - 1 do
        let a = Array.init nTrain id
        for i in nTrain - 1 .. -1 .. 1 do
            let j = rng.Next(i + 1)
            let t = a.[i] in a.[i] <- a.[j]; a.[j] <- t
        for i in 0 .. nTrain - 1 do p.[ep * nTrain + i] <- int64 a.[i]
    p

// Fresh verification rotations, row-major 3x3, from the ML oracle's sampler.
let rot3 =
    let rng = Random(seedRotVerify)
    let out = Array.zeroCreate<float> (nRot * 9)
    for k in 0 .. nRot - 1 do
        let r = Rotations.randomRotation rng
        for i in 0 .. 2 do
            for j in 0 .. 2 do
                out.[k * 9 + i * 3 + j] <- r.[i].[j]
    out
let rotResidual =
    [| for k in 0 .. nRot - 1 do
         for i in 0 .. 2 do
           for j in 0 .. 2 ->
             let mutable s = 0.0
             for c in 0 .. 2 do s <- s + rot3.[k * 9 + i * 3 + c] * rot3.[k * 9 + j * 3 + c]
             abs (s - (if i = j then 1.0 else 0.0)) |]
    |> Array.max
if rotResidual > 1e-12 then failwithf "verification rotations not orthogonal (%g)" rotResidual

// ---------------------------------------------------------------------------
// Write the store (idempotent: replaced wholesale).
// ---------------------------------------------------------------------------
let root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))
let store = Path.Combine(root, "examples", "data", "moments.zarr")
if Directory.Exists store then Directory.Delete(store, true)

let fvar name dims (shape: int list) (data: float[]) : ZarrWrite.WriteVar =
    { Name = name
      DimNames = Some dims
      Shape = shape |> List.map int64
      Chunks = shape |> List.map int64
      FillValue = FillFloat 0.0
      Data = ZarrWrite.WF64 data
      OmitChunks = []
      Blade = None }
let ivar name dims (shape: int list) (data: int64[]) : ZarrWrite.WriteVar =
    { Name = name
      DimNames = Some dims
      Shape = shape |> List.map int64
      Chunks = shape |> List.map int64
      FillValue = FillInt 0L
      Data = ZarrWrite.WI64 data
      OmitChunks = []
      Blade = None }

ZarrWrite.writeStoreV3 store
    [ fvar "pos_train" [ "samp_tr"; "pt"; "xyz" ] [ nTrain; nPoint; 3 ] posTrain
      fvar "pos_train_yzx" [ "samp_tr"; "pt"; "xyz" ] [ nTrain; nPoint; 3 ] posTrainYzx
      fvar "pos_test" [ "samp_te"; "pt"; "xyz" ] [ nTest; nPoint; 3 ] posTest
      fvar "pos_test_yzx" [ "samp_te"; "pt"; "xyz" ] [ nTest; nPoint; 3 ] posTestYzx
      fvar "y_train" [ "samp_tr"; "klass" ] [ nTrain; nClass ] yTrain
      fvar "y_test" [ "samp_te"; "klass" ] [ nTest; nClass ] yTest
      ivar "label_train" [ "samp_tr" ] [ nTrain ] labTrain
      ivar "label_test" [ "samp_te" ] [ nTest ] labTest
      ivar "perm" [ "epoch"; "samp_tr" ] [ nEpochPerm; nTrain ] perm
      fvar "rot3" [ "rotk"; "nine" ] [ nRot; 9 ] rot3
      fvar "demo_icosa" [ "pt"; "xyz" ] [ nPoint; 3 ] demoFlat
      fvar "demo_icosa_yzx" [ "pt"; "xyz" ] [ nPoint; 3 ] demoFlatYzx
      fvar "ref_radii" [ "pt" ] [ nPoint ] refRadii
      fvar "shell_mu" [ "shell" ] [ 5 ] shellMu
      fvar "shell_sg" [ "shell" ] [ 5 ] shellSg ]

// Deterministic content hash of the whole store: sorted relative paths, then
// path bytes + file bytes through one SHA-256. Two runs must print the same.
let storeHash =
    use sha = SHA256.Create()
    let files =
        Directory.GetFiles(store, "*", SearchOption.AllDirectories)
        |> Array.map (fun f -> f.Substring(store.Length).Replace('\\', '/'))
        |> Array.sort
    use ms = new MemoryStream()
    for f in files do
        let pb = Text.Encoding.UTF8.GetBytes(f)
        ms.Write(pb, 0, pb.Length)
        let fb = File.ReadAllBytes(Path.Combine(store, f.TrimStart('/')))
        ms.Write(fb, 0, fb.Length)
    sha.ComputeHash(ms.ToArray()) |> Array.map (sprintf "%02x") |> String.concat ""

// ---------------------------------------------------------------------------
// The pins the notebook relies on.
// ---------------------------------------------------------------------------
let clsName = [| "iso"; "octa"; "tetra"; "heavy" |]
let sumsq (a: float[]) = a |> Array.sumBy (fun v -> v * v)
printfn "wrote %s" store
printfn "  store hash = %s" storeHash
printfn "  classes=%d samples/class=%d (train %d + test %d) points=%d" nClass (nTrainPer + nTestPer) nTrainPer nTestPer nPoint
printfn "  knobs: octa c=%g tau=%g | tetra c=%g tau=%g | heavy p=%g s=%g" octaC octaTau tetraC tetraTau heavyPHi heavySHi
let mnIt, mxIt, avIt = matchIterStats
printfn "  quantile-match iterations: min %d max %d mean %.1f; converged residual max = %g" mnIt mxIt avIt matchResidualMax
printfn "  WHITENING (exact, last): max |K1| = %g   max |K2 - I| = %g  (every stored sample)" k1ResMax k2ResMax
printfn "  radial match after rotation+polish: max |sorted radii - r*| = %g" finalRadialResMax
printfn "  K=0 shell occupancies: matched max dev = %g   heavy max dev = %g" occDevMatched occDevHeavy
printfn "  shell mu = %s" (String.Join(", ", shellMu |> Array.map (sprintf "%.17g")))
printfn "  shell sg = %.17g (all five)" shellSg.[0]
printfn "  per-class separation diagnostics (mean +- sd over the 150 unrotated processed clouds):"
for cls in 0 .. nClass - 1 do
    let (m4, s4) = classStat a4 cls
    let (m3, s3) = classStat a3 cls
    let (mr, sr) = classStat r4 cls
    printfn "    %-5s a4 = %.5f +- %.5f | a3 = %+.5f +- %.5f | mean r^4 = %.5f +- %.6f"
        clsName.[cls] m4 s4 m3 s3 mr sr
printfn "  z-separations of each class's OWN block against iso:"
printfn "    octa  a4 z = %.1f  (K4 l=4)" (zsep (classStat a4 1) (classStat a4 0))
printfn "    tetra a3 z = %.1f  (K3)" (zsep (classStat a3 2) (classStat a3 0))
printfn "    heavy r4 z = %.1f  (K4 l=0)" (zsep (classStat r4 3) (classStat r4 0))
printfn "  icosa demo a4 = %.6f (iso expectation 0.6: l<=4-invisible by construction)"
    (a4 demoIcosa)
printfn "  pos_train[0,0,:] = %.17g %.17g %.17g" posTrain.[0] posTrain.[1] posTrain.[2]
printfn "  pos_test[0,0,:]  = %.17g %.17g %.17g" posTest.[0] posTest.[1] posTest.[2]
printfn "  sum(pos_train^2) = %.17g   sum(pos_test^2) = %.17g" (sumsq posTrain) (sumsq posTest)
printfn "  perm[0,0..7] = %s" (String.Join(", ", Array.sub perm 0 8 |> Array.map string))
printfn "  rot3 orthogonality residual = %g" rotResidual
