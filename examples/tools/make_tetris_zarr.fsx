// make_tetris_zarr.fsx — deterministic fixture generator for the Tetris
// deduction notebook (examples/tetris_shapes.bladenb).
//
// Bakes the eight e3nn tetromino shapes, rotated by seeded Haar samples, into
// a committed zarr v3 store at examples/data/tetris.zarr, reusing the repo's
// own machinery end to end:
//   - ZarrWrite.writeStoreV3 (src/providers/ZarrProvider.fs) writes the store,
//   - BladeML.Rotations.randomRotation (oracles/ml/Rotations.fs) — the SAME
//     rotation sampler the ML oracle's equivariance tests use — supplies the
//     orientations, driven by System.Random with fixed seeds.
//
// The raw shapes come from C:\Users\cdupu\Data\tetris\pieces.csv (the eight
// canonical e3nn tetrominoes, 4 integer points each). Shapes 0 and 1 —
// chiral_shape_1 and chiral_shape_2 — are a MIRROR PAIR: shape 1 is shape 0
// with y negated, point order preserved. No rotation carries one into the
// other, which is the whole subject of the notebook.
//
// Everything downstream is pre-shaped here, because Blade's reverse-mode AD
// bans combinators inside a differentiated loss: the store holds centered
// coordinates and a one-hot target table, so the loss is straight-line reads.
//
// Run from anywhere:  dotnet fsi examples/tools/make_tetris_zarr.fsx
// (Regenerates the committed store in place; idempotent — no RNG state
// crosses a run boundary, so a second run rewrites identical bytes.)
// Override the raw-input location with BLADE_TETRIS_CSV if it lives elsewhere.

#r "System.Security.Cryptography"
// The prefix of Blade.fsproj's compile order that IR.fs and ZarrProvider need,
// in that order -- fsi has no project file, so the dependency chain is spelled
// out here, exactly as make_qg_zarr.fsx does. (Blade.fsproj is the source of
// truth if this ever drifts.)
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
// The rotation sampler, from the BladeML oracle project. Its compile order is
// oracles/ml/BladeML.fsproj's prefix through Rotations.fs (MLSpec is shared
// with the compiler and comes first there too).
#load "../../src/ml/compiler/MLSpec.fs"
#load "../../oracles/ml/MathUtils.fs"
#load "../../oracles/ml/Irreps.fs"
#load "../../oracles/ml/IrrepsIdx.fs"
#load "../../oracles/ml/Wigner.fs"
#load "../../oracles/ml/SphericalHarmonics.fs"
#load "../../oracles/ml/Rotations.fs"

open System
open System.IO
open Blade.ZarrProvider
open BladeML

// ---------------------------------------------------------------------------
// Sizes. Deliberately non-power-of-two where a fold walks them.
// ---------------------------------------------------------------------------
let nShape = 8
let nPoint = 4
let nRotTrain = 12          // orientations per shape in the training set
let nRotTest = 6            // FRESH orientations per shape in the test set
let nTrain = nShape * nRotTrain
let nTest = nShape * nRotTest

let seedTrain = 20260826
let seedTest = 8082026

// ---------------------------------------------------------------------------
// Raw shapes: pieces.csv, header shape_index,shape_name,point_index,x,y,z.
// ---------------------------------------------------------------------------
let csvPath =
    match Environment.GetEnvironmentVariable "BLADE_TETRIS_CSV" with
    | null | "" -> @"C:\Users\cdupu\Data\tetris\pieces.csv"
    | p -> p

if not (File.Exists csvPath) then
    failwithf "tetris pieces.csv not found at %s (set BLADE_TETRIS_CSV)" csvPath

let rawRows =
    File.ReadAllLines csvPath
    |> Array.skip 1
    |> Array.filter (fun l -> l.Trim() <> "")
    |> Array.map (fun l ->
        let f = l.Split(',')
        (int f.[0], f.[1], int f.[2], float f.[3], float f.[4], float f.[5]))

let shapeNames =
    [| for s in 0 .. nShape - 1 ->
         rawRows |> Array.find (fun (si, _, _, _, _, _) -> si = s) |> fun (_, n, _, _, _, _) -> n |]

/// Raw coordinates, [shape].[point*3 + c].
let rawShape =
    [| for s in 0 .. nShape - 1 ->
         let pts = rawRows |> Array.filter (fun (si, _, _, _, _, _) -> si = s)
                           |> Array.sortBy (fun (_, _, pi, _, _, _) -> pi)
         if pts.Length <> nPoint then failwithf "shape %d has %d points" s pts.Length
         [| for (_, _, _, x, y, z) in pts do yield! [| x; y; z |] |] |]

/// Centered coordinates (centroid at the origin) — translation is not part of
/// the symmetry the notebook certifies, so it is removed once, here.
let canon =
    rawShape
    |> Array.map (fun pts ->
        let cx = (Array.init nPoint (fun p -> pts.[p * 3 + 0]) |> Array.sum) / float nPoint
        let cy = (Array.init nPoint (fun p -> pts.[p * 3 + 1]) |> Array.sum) / float nPoint
        let cz = (Array.init nPoint (fun p -> pts.[p * 3 + 2]) |> Array.sum) / float nPoint
        [| for p in 0 .. nPoint - 1 do
             yield pts.[p * 3 + 0] - cx
             yield pts.[p * 3 + 1] - cy
             yield pts.[p * 3 + 2] - cz |])

// The mirror-pair assertion: shape 1 must be shape 0 with y negated, point
// order preserved. If pieces.csv is ever re-exported in another order this
// fires instead of silently producing a notebook whose punchline is false.
let mirrorResidual =
    Array.init (nPoint * 3) (fun k ->
        let sgn = if k % 3 = 1 then -1.0 else 1.0
        abs (canon.[1].[k] - sgn * canon.[0].[k]))
    |> Array.max
if mirrorResidual > 1e-12 then
    failwithf "shapes 0/1 are not a y-mirror pair (residual %g)" mirrorResidual

/// Signed volume of the ORDERED tetrahedron (p1-p0, p2-p0, p3-p0): a
/// pseudoscalar, but one that depends on the point order, so it is a witness
/// and not a verdict. Zero exactly on the planar shapes.
let signedVolume (pts: float[]) =
    let d i c = pts.[i * 3 + c] - pts.[0 * 3 + c]
    let a = [| d 1 0; d 1 1; d 1 2 |]
    let b = [| d 2 0; d 2 1; d 2 2 |]
    let c = [| d 3 0; d 3 1; d 3 2 |]
    a.[0] * (b.[1] * c.[2] - b.[2] * c.[1])
    + a.[1] * (b.[2] * c.[0] - b.[0] * c.[2])
    + a.[2] * (b.[0] * c.[1] - b.[1] * c.[0])

/// The 24 proper rotations of the cube (signed permutation matrices, det +1).
/// Every tetromino is a lattice shape, so if its mirror image is a rotation of
/// some tetromino at all, the rotation lies in this group — which makes the
/// chirality verdict below EXACT arithmetic rather than a tolerance.
let octahedral =
    [| for perm in [ [|0;1;2|]; [|0;2;1|]; [|1;0;2|]; [|1;2;0|]; [|2;0;1|]; [|2;1;0|] ] do
         for sx in [ 1.0; -1.0 ] do
           for sy in [ 1.0; -1.0 ] do
             for sz in [ 1.0; -1.0 ] do
               let s = [| sx; sy; sz |]
               let m = Array2D.init 3 3 (fun i j -> if perm.[i] = j then s.[i] else 0.0)
               let det =
                   m.[0,0] * (m.[1,1] * m.[2,2] - m.[1,2] * m.[2,1])
                   - m.[0,1] * (m.[1,0] * m.[2,2] - m.[1,2] * m.[2,0])
                   + m.[0,2] * (m.[1,0] * m.[2,1] - m.[1,1] * m.[2,0])
               if det > 0.0 then yield m |]

/// Point SETS, compared without regard to order (the featurization pools over
/// points, so the network sees a set, never a list).
let sameSet (a: float[]) (b: float[]) =
    let pt (v: float[]) i = (v.[i * 3], v.[i * 3 + 1], v.[i * 3 + 2])
    let key (v: float[]) =
        List.init nPoint (pt v) |> List.sortBy (fun (x, y, z) -> (x, y, z))
    let close (x1: float, y1: float, z1: float) (x2: float, y2: float, z2: float) =
        abs (x1 - x2) < 1e-12 && abs (y1 - y2) < 1e-12 && abs (z1 - z2) < 1e-12
    List.forall2 close (key a) (key b)

let applyMat (m: float[,]) (pts: float[]) =
    [| for p in 0 .. nPoint - 1 do
         for i in 0 .. 2 ->
           m.[i,0] * pts.[p * 3] + m.[i,1] * pts.[p * 3 + 1] + m.[i,2] * pts.[p * 3 + 2] |]

/// Which shape the mirror image of shape s is (as a point set, up to proper
/// rotation). s -> s means achiral; s -> t <> s is the chiral pair.
let mirrorPartner (s: int) =
    let mirrored = canon.[s] |> Array.mapi (fun k v -> if k % 3 = 0 then -v else v)
    [ 0 .. nShape - 1 ]
    |> List.tryFind (fun t ->
        octahedral |> Array.exists (fun m -> sameSet (applyMat m mirrored) canon.[t]))
    |> Option.defaultValue -1

// ---------------------------------------------------------------------------
// Rotated sample sets. Shape-major: sample s*nRot + k is shape s under its
// k-th orientation, so a per-class fold walks contiguous blocks.
// ---------------------------------------------------------------------------
let rotateSet (seed: int) (nRot: int) =
    let rng = Random(seed)
    let pos = Array.zeroCreate<float> (nShape * nRot * nPoint * 3)
    let lab = Array.zeroCreate<int64> (nShape * nRot)
    for s in 0 .. nShape - 1 do
        for k in 0 .. nRot - 1 do
            let r = Rotations.randomRotation rng
            let smp = s * nRot + k
            lab.[smp] <- int64 s
            for p in 0 .. nPoint - 1 do
                let v = [| canon.[s].[p * 3 + 0]; canon.[s].[p * 3 + 1]; canon.[s].[p * 3 + 2] |]
                let w = MathUtils.matVec r v
                for c in 0 .. 2 do
                    pos.[(smp * nPoint + p) * 3 + c] <- w.[c]
    pos, lab

let posTrain, labTrain = rotateSet seedTrain nRotTrain
let posTest, labTest = rotateSet seedTest nRotTest

/// One-hot targets, (sample, class) row-major — the loss reads them directly,
/// so no `if` and no integer gather is needed inside the differentiated body.
let oneHot (lab: int64[]) =
    let y = Array.zeroCreate<float> (lab.Length * nShape)
    lab |> Array.iteri (fun i l -> y.[i * nShape + int l] <- 1.0)
    y

let yTrain = oneHot labTrain
let yTest = oneHot labTest

let piecesFlat = Array.concat canon

// ---------------------------------------------------------------------------
// Write the store (idempotent: replaced wholesale).
// ---------------------------------------------------------------------------
let root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))
let store = Path.Combine(root, "examples", "data", "tetris.zarr")
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
    [ fvar "pieces" [ "piece"; "pt"; "xyz" ] [ nShape; nPoint; 3 ] piecesFlat
      fvar "pos_train" [ "samp_tr"; "pt"; "xyz" ] [ nTrain; nPoint; 3 ] posTrain
      fvar "y_train" [ "samp_tr"; "klass" ] [ nTrain; nShape ] yTrain
      ivar "label_train" [ "samp_tr" ] [ nTrain ] labTrain
      fvar "pos_test" [ "samp_te"; "pt"; "xyz" ] [ nTest; nPoint; 3 ] posTest
      fvar "y_test" [ "samp_te"; "klass" ] [ nTest; nShape ] yTest
      ivar "label_test" [ "samp_te" ] [ nTest ] labTest ]

// ---------------------------------------------------------------------------
// The pins the notebook relies on.
// ---------------------------------------------------------------------------
printfn "wrote %s" store
printfn "  shapes=%d points=%d  train=%d (%d rot/shape, seed %d)  test=%d (%d rot/shape, seed %d)"
    nShape nPoint nTrain nRotTrain seedTrain nTest nRotTest seedTest
printfn "  mirror-pair residual (shape 1 vs y-negated shape 0) = %g" mirrorResidual
printfn "  per shape: mirror partner (as a point SET, up to proper rotation) and"
printfn "  the ordered signed tetrahedron volume:"
for s in 0 .. nShape - 1 do
    let t = mirrorPartner s
    let verdict = if t = s then "achiral" else sprintf "CHIRAL, mirror of %d" t
    printfn "    %d %-15s -> %-20s vol = %+.17g" s shapeNames.[s] verdict (signedVolume canon.[s])
printfn "  pieces[0] (chiral_shape_1, centered) = %s"
    (String.Join(", ", canon.[0] |> Array.map (sprintf "%.2f")))
printfn "  pieces[1] (chiral_shape_2, centered) = %s"
    (String.Join(", ", canon.[1] |> Array.map (sprintf "%.2f")))
printfn "  pos_train[0,0,:] = %.17g %.17g %.17g" posTrain.[0] posTrain.[1] posTrain.[2]
printfn "  pos_test[0,0,:]  = %.17g %.17g %.17g" posTest.[0] posTest.[1] posTest.[2]
let sumsq (a: float[]) = a |> Array.sumBy (fun v -> v * v)
printfn "  sum(pos_train^2) = %.17g   sum(pos_test^2) = %.17g" (sumsq posTrain) (sumsq posTest)
