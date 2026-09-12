// make_aspirin_zarr.fsx -- deterministic fixture generator for the MD17
// aspirin benchmark notebook (examples/aspirin_energy.bladenb).
//
// Reads the raw sGDML `.npy` members of the aspirin CCSD benchmark from
// C:\Users\cdupu\Data\md17_aspirin\ and bakes a low-data training problem into
// a committed zarr v3 store at examples/data/aspirin.zarr, reusing the repo's
// own machinery end to end:
//   - ZarrWrite.writeStoreV3 (src/providers/ZarrProvider.fs) writes the store,
//   - BladeML.Rotations.randomRotation (oracles/ml/Rotations.fs) -- the SAME
//     rotation sampler the ML oracle's equivariance tests use -- supplies the
//     verification rotations, driven by System.Random with fixed seeds.
// The .npy reader below is hand-written against the format spec (BCL only): a
// `\x93NUMPY` magic, a version pair, a header length, and an ASCII Python dict.
// No new package reference; the repo's only NuGet dep stays ZstdSharp.Port.
//
// THE FORTRAN-ORDER TRAP. `R` and `F` are float64 (N, 21, 3) written with
// `fortran_order: True` -- COLUMN-MAJOR, so element (n, a, c) lives at flat
// slot n + N*a + N*21*c, not at (n*21 + a)*3 + c. Reading them C-major does
// not fail, it silently produces a different molecule. Three independent
// checks below refuse to let that happen quietly:
//   1. the generic reader's transposed value for R[0,0,:] is compared against
//      a SEPARATE hand-decoded read that does the stride arithmetic inline;
//   2. both readings are printed side by side, so the difference is visible;
//   3. a PHYSICAL check -- under the correct reading every atom has a bonded
//      neighbour inside 1.8 A and the C-C bonds land near 1.4 A; under the
//      C-major misreading the connectivity test fails outright. That check is
//      an assertion, not a comment: the script stops rather than write a store
//      whose molecule is scrambled.
//
// Everything downstream is pre-shaped here, because Blade's reverse-mode AD
// bans combinators inside a differentiated loss: the store holds centered
// coordinates, standardized energies, a float species table (so per-species
// pooling is a branch-free weighted sum), and a per-epoch permutation table
// (the language has no rand.shuffle), so the loss is straight-line reads.
//
// Run from anywhere:  dotnet fsi examples/tools/make_aspirin_zarr.fsx
// (Regenerates the committed store in place; idempotent -- no RNG state
// crosses a run boundary, so a second run rewrites identical bytes.)
// Override the raw-input location with BLADE_ASPIRIN_DIR if it lives elsewhere.

#r "System.Security.Cryptography"
// The prefix of Blade.fsproj's compile order that IR.fs and ZarrProvider need,
// in that order -- fsi has no project file, so the dependency chain is spelled
// out here, exactly as make_qg_zarr.fsx and make_tetris_zarr.fsx do.
// (Blade.fsproj is the source of truth if this ever drifts.)
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
// A .npy reader, from the format spec. ~60 lines, BCL only.
// ---------------------------------------------------------------------------

/// What the header dict said, plus where the raw element bytes start.
type NpyHeader =
    { Descr: string           // e.g. "<f8", "|u1", "|S12"
      Fortran: bool
      Shape: int list
      DataStart: int }

/// Pull `key`'s value text out of the ASCII Python dict, without a JSON parser
/// (the header is not JSON: single quotes, `True`, and a trailing-comma tuple).
let private dictField (hdr: string) (key: string) : string =
    let k = "'" + key + "'"
    let i = hdr.IndexOf k
    if i < 0 then failwithf "npy header has no '%s': %s" key hdr
    let j = hdr.IndexOf(':', i + k.Length)
    if j < 0 then failwithf "npy header field '%s' has no ':': %s" key hdr
    // The value runs to the comma that closes it at brace/paren depth 0.
    let mutable depth = 0
    let mutable p = j + 1
    let mutable stop = -1
    while stop < 0 && p < hdr.Length do
        match hdr.[p] with
        | '(' | '[' | '{' -> depth <- depth + 1
        | ')' | ']' -> depth <- depth - 1
        | '}' -> if depth = 0 then stop <- p else depth <- depth - 1
        | ',' when depth = 0 -> stop <- p
        | _ -> ()
        p <- p + 1
    let stop = if stop < 0 then hdr.Length else stop
    hdr.Substring(j + 1, stop - j - 1).Trim()

let readNpyHeader (raw: byte[]) : NpyHeader =
    if raw.Length < 10 then failwith "npy file is too short"
    if raw.[0] <> 0x93uy || raw.[1] <> byte 'N' || raw.[2] <> byte 'U'
       || raw.[3] <> byte 'M' || raw.[4] <> byte 'P' || raw.[5] <> byte 'Y' then
        failwith "not a .npy file (bad \\x93NUMPY magic)"
    let major = int raw.[6]
    let hlen, dataStart =
        if major = 1 then int (BitConverter.ToUInt16(raw, 8)), 10
        else int (BitConverter.ToUInt32(raw, 8)), 12
    let hdr = Text.Encoding.ASCII.GetString(raw, dataStart, hlen)
    let shapeTxt = (dictField hdr "shape").Trim([| '('; ')' |])
    let shape =
        shapeTxt.Split(',')
        |> Array.map (fun s -> s.Trim())
        |> Array.filter (fun s -> s <> "")
        |> Array.map int
        |> List.ofArray
    { Descr = (dictField hdr "descr").Trim('\'')
      Fortran = (dictField hdr "fortran_order") = "True"
      Shape = shape
      DataStart = dataStart + hlen }

/// Numeric .npy -> float[] in C (row-major) order, whatever the file's order
/// was. `fortran_order: True` is honoured by walking the C-order destination
/// and computing the COLUMN-MAJOR source slot for each element.
let readNpyF64 (path: string) : NpyHeader * float[] =
    let raw = File.ReadAllBytes path
    let h = readNpyHeader raw
    if h.Descr <> "<f8" then failwithf "%s: expected '<f8', got '%s'" path h.Descr
    let dims = List.toArray h.Shape
    let n = dims |> Array.fold (*) 1
    if raw.Length - h.DataStart < n * 8 then failwithf "%s: truncated payload" path
    let out = Array.zeroCreate<float> n
    if not h.Fortran then
        Buffer.BlockCopy(raw, h.DataStart, out, 0, n * 8)
    else
        // Column-major strides: stride[0] = 1, stride[k] = stride[k-1]*dims[k-1].
        let fstride = Array.zeroCreate<int> dims.Length
        let mutable s = 1
        for k in 0 .. dims.Length - 1 do
            fstride.[k] <- s
            s <- s * dims.[k]
        let idx = Array.zeroCreate<int> dims.Length
        for c in 0 .. n - 1 do
            // idx = the C-order multi-index of destination slot c.
            let mutable rem = c
            for k in dims.Length - 1 .. -1 .. 0 do
                idx.[k] <- rem % dims.[k]
                rem <- rem / dims.[k]
            let mutable src = 0
            for k in 0 .. dims.Length - 1 do
                src <- src + idx.[k] * fstride.[k]
            out.[c] <- BitConverter.ToDouble(raw, h.DataStart + src * 8)
    h, out

let readNpyU8 (path: string) : NpyHeader * byte[] =
    let raw = File.ReadAllBytes path
    let h = readNpyHeader raw
    if h.Descr <> "|u1" then failwithf "%s: expected '|u1', got '%s'" path h.Descr
    let n = h.Shape |> List.fold (*) 1
    h, Array.sub raw h.DataStart n

// ---------------------------------------------------------------------------
// Inputs.
// ---------------------------------------------------------------------------
let rawDir =
    match Environment.GetEnvironmentVariable "BLADE_ASPIRIN_DIR" with
    | null | "" -> @"C:\Users\cdupu\Data\md17_aspirin"
    | p -> p

let need name =
    let p = Path.Combine(rawDir, name)
    if not (File.Exists p) then
        failwithf "aspirin raw file not found: %s (set BLADE_ASPIRIN_DIR)" p
    p

let hTrR, trR = readNpyF64 (need "train_R.npy")
let hTrE, trE = readNpyF64 (need "train_E.npy")
let hTrF, trF = readNpyF64 (need "train_F.npy")
let hTeR, teR = readNpyF64 (need "test_R.npy")
let hTeE, teE = readNpyF64 (need "test_E.npy")
let hTeF, teF = readNpyF64 (need "test_F.npy")
let hTrZ, trZ = readNpyU8 (need "train_z.npy")
let hTeZ, teZ = readNpyU8 (need "test_z.npy")

let nAtom = 21
let nRawTrain = hTrR.Shape.Head
let nRawTest = hTeR.Shape.Head

if hTrR.Shape <> [ nRawTrain; nAtom; 3 ] then failwithf "train_R shape %A" hTrR.Shape
if hTeR.Shape <> [ nRawTest; nAtom; 3 ] then failwithf "test_R shape %A" hTeR.Shape
if hTrE.Shape <> [ nRawTrain; 1 ] then failwithf "train_E shape %A" hTrE.Shape
if hTeE.Shape <> [ nRawTest; 1 ] then failwithf "test_E shape %A" hTeE.Shape
if hTrZ.Shape <> [ nAtom ] then failwithf "train_z shape %A" hTrZ.Shape
if trZ <> teZ then failwith "train_z and test_z disagree -- different molecules?"
// The benchmark's own arrays: energies C-order, coordinates/forces Fortran.
if hTrE.Fortran || hTeE.Fortran then failwith "E was expected C-order"
if not (hTrR.Fortran && hTeR.Fortran && hTrF.Fortran && hTeF.Fortran) then
    failwith "R/F were expected fortran_order: True -- schema drift, stop"

// Aspirin is C9 O4 H8. Assert the composition rather than trust the filename.
let nC = trZ |> Array.filter ((=) 6uy) |> Array.length
let nO = trZ |> Array.filter ((=) 8uy) |> Array.length
let nH = trZ |> Array.filter ((=) 1uy) |> Array.length
if (nC, nO, nH) <> (9, 4, 8) then
    failwithf "expected aspirin C9 O4 H8, got C%d O%d H%d" nC nO nH
if nC + nO + nH <> nAtom then failwithf "unknown species in z: %A" trZ

// ---------------------------------------------------------------------------
// THE FORTRAN-ORDER PROOF. Three checks, all fatal.
// ---------------------------------------------------------------------------
let rawTrainBytes = File.ReadAllBytes (need "train_R.npy")

/// Hand-decoded, straight from the file bytes, with the column-major stride
/// arithmetic written out inline -- deliberately NOT sharing code with
/// readNpyF64, so agreement between the two is evidence and not tautology.
let handFortran (n: int) (a: int) (c: int) =
    let slot = n + nRawTrain * a + nRawTrain * nAtom * c
    BitConverter.ToDouble(rawTrainBytes, hTrR.DataStart + slot * 8)

/// The same element under the naive C-major misreading -- what a reader that
/// ignores `fortran_order` would have returned.
let handCMajor (n: int) (a: int) (c: int) =
    let slot = (n * nAtom + a) * 3 + c
    BitConverter.ToDouble(rawTrainBytes, hTrR.DataStart + slot * 8)

let genericAtom0 = [| trR.[0]; trR.[1]; trR.[2] |]
let handAtom0 = [| handFortran 0 0 0; handFortran 0 0 1; handFortran 0 0 2 |]
let wrongAtom0 = [| handCMajor 0 0 0; handCMajor 0 0 1; handCMajor 0 0 2 |]

let atom0Residual =
    Array.map2 (fun (a: float) b -> abs (a - b)) genericAtom0 handAtom0 |> Array.max
if atom0Residual > 0.0 then
    failwithf "reader disagrees with the hand-decoded fortran read (residual %g)" atom0Residual

/// Chemistry, used as a decoder test. Given a coordinate accessor
/// (sample -> atom -> component), report two numbers about the geometry it
/// claims: the nearest-neighbour distance band over sample 0's 21 atoms, and
/// how much each of those nearest-neighbour distances MOVES across the first
/// 50 samples. A real molecule answers "every atom has a partner between 0.9
/// and 1.65 A" (O-H at 0.97 up to C-C at 1.5) and "those bonds are rigid to a
/// few hundredths of an angstrom" -- a thermal ensemble of one covalent
/// structure. Nothing else does.
let geomStats (get: int -> int -> int -> float) =
    let d n a b =
        let dx = get n a 0 - get n b 0
        let dy = get n a 1 - get n b 1
        let dz = get n a 2 - get n b 2
        sqrt (dx * dx + dy * dy + dz * dz)
    let nn =
        [| for a in 0 .. nAtom - 1 ->
             [| for b in 0 .. nAtom - 1 do if b <> a then yield (d 0 a b, b) |]
             |> Array.minBy fst |]
    let spread =
        [| for a in 0 .. nAtom - 1 ->
             let b = snd nn.[a]
             let ds = [| for n in 0 .. 49 -> d n a b |]
             let mu = Array.average ds
             sqrt ((ds |> Array.sumBy (fun v -> (v - mu) * (v - mu))) / 50.0) |]
    (nn |> Array.map fst |> Array.min), (nn |> Array.map fst |> Array.max), Array.max spread

let (nnMinF, nnMaxF, spreadF) = geomStats handFortran
let (nnMinC, nnMaxC, spreadC) = geomStats handCMajor
if nnMinF < 0.9 || nnMaxF > 1.65 || spreadF > 0.15 then
    failwithf "the column-major reading is not a covalent molecule (nn %g..%g A, spread %g A)"
        nnMinF nnMaxF spreadF
if nnMinC >= 0.9 && nnMaxC <= 1.65 && spreadC <= 0.15 then
    failwith "the C-major misreading ALSO looks like a molecule -- the proof is vacuous, investigate"

// ---------------------------------------------------------------------------
// The low-data subset. A DECIMATION, not a prefix: every 10th frame of the
// benchmark's 1000-frame train split and every 5th of its 500-frame test
// split. Both subsets therefore span the whole trajectory each split covers,
// which is what makes the two energy distributions comparable -- the first 100
// frames of each split are two different stretches of molecular dynamics, and
// standardizing one by the other's statistics leaves a mean offset no model
// can learn away. Deterministic, seedless, and reproducible from the stride
// alone.
// ---------------------------------------------------------------------------
let nTrain = 500
let nTest = 100
let strideTrain = nRawTrain / nTrain
let strideTest = nRawTest / nTest
if nRawTrain < nTrain || nRawTest < nTest then failwith "raw split too small"
let trainIdx = Array.init nTrain (fun i -> i * strideTrain)
let testIdx = Array.init nTest (fun i -> i * strideTest)

/// Gather the chosen frames AND remove each one's centroid: the featurization
/// expands every atom about the molecular centre, and translation is not part
/// of the symmetry the notebook certifies, so it goes once, here.
let centered (src: float[]) (idx: int[]) =
    let out = Array.zeroCreate<float> (idx.Length * nAtom * 3)
    idx |> Array.iteri (fun n s ->
        let dst = n * nAtom * 3
        let bas = s * nAtom * 3
        for c in 0 .. 2 do
            let mutable acc = 0.0
            for a in 0 .. nAtom - 1 do acc <- acc + src.[bas + a * 3 + c]
            let mu = acc / float nAtom
            for a in 0 .. nAtom - 1 do
                out.[dst + a * 3 + c] <- src.[bas + a * 3 + c] - mu)
    out

let posTrain = centered trR trainIdx
let posTest = centered teR testIdx

/// Forces are already translation-invariant, so they are gathered, not centered.
let gather3 (src: float[]) (idx: int[]) =
    let out = Array.zeroCreate<float> (idx.Length * nAtom * 3)
    idx |> Array.iteri (fun n s ->
        Array.blit src (s * nAtom * 3) out (n * nAtom * 3) (nAtom * 3))
    out
let fTest = gather3 teF testIdx
let fTrain = gather3 trF trainIdx

/// The (y, z, x) permuted copy. `ml.y_to` takes cartesian (x, y, z) and does
/// this permutation internally, but the raw l = 1 basis order in the irreps
/// machinery IS (y, z, x) (src/ml/compiler/MLElaborate.fs:217-219), so anything
/// fed to `ml.derive_poly` or compared against an l = 1 block directly wants
/// this copy. `aspirin_energy.bladenb` prints one row of it and moves on;
/// anything feeding `ml.derive_poly` raw consumes it wholesale.
let yzx (src: float[]) (nSamp: int) =
    let out = Array.zeroCreate<float> (nSamp * nAtom * 3)
    for i in 0 .. nSamp * nAtom - 1 do
        out.[i * 3 + 0] <- src.[i * 3 + 1]
        out.[i * 3 + 1] <- src.[i * 3 + 2]
        out.[i * 3 + 2] <- src.[i * 3 + 0]
    out

let posTrainYzx = yzx posTrain nTrain
let posTestYzx = yzx posTest nTest

// ---------------------------------------------------------------------------
// Energies. MD17 CCSD energies are in kcal/mol and sit near -406000, so a
// regression target has to be standardized: Grad.fs:110 refuses unit-carrying
// losses, and a 1e6-scale target would swamp any sane learning rate anyway.
// Statistics come from the TRAIN subset only; the test split is standardized
// with the SAME two numbers, which is what makes the reported MAE honest.
// ---------------------------------------------------------------------------
let eTrainRaw = trainIdx |> Array.map (fun s -> trE.[s])
let eTestRaw = testIdx |> Array.map (fun s -> teE.[s])
let eMean = Array.average eTrainRaw
let eStd =
    sqrt ((eTrainRaw |> Array.sumBy (fun v -> (v - eMean) * (v - eMean))) / float nTrain)
let eTrain = eTrainRaw |> Array.map (fun v -> (v - eMean) / eStd)
let eTest = eTestRaw |> Array.map (fun v -> (v - eMean) / eStd)

/// The radial width for the exp(-r^2/sigma^2) channel: the RMS atomic radius
/// of the training subset, so the Gaussian is centred on the data's own scale
/// instead of a magic constant.
let sigma =
    let mutable s = 0.0
    for n in 0 .. nTrain - 1 do
        for a in 0 .. nAtom - 1 do
            let b = (n * nAtom + a) * 3
            s <- s + posTrain.[b] * posTrain.[b] + posTrain.[b+1] * posTrain.[b+1]
                   + posTrain.[b+2] * posTrain.[b+2]
    sqrt (s / float (nTrain * nAtom))

// ---------------------------------------------------------------------------
// Species. A FLOAT one-hot table, not an integer label: the notebook pools
// per species inside a reverse-differentiated body, where a branch on the
// atomic number would be a refusal and a weighted sum is straight-line
// arithmetic. Column order is (C, O, H).
// ---------------------------------------------------------------------------
let specCol (z: byte) = match z with | 6uy -> 0 | 8uy -> 1 | 1uy -> 2 | _ -> -1
let specW =
    let w = Array.zeroCreate<float> (nAtom * 3)
    trZ |> Array.iteri (fun a z -> w.[a * 3 + specCol z] <- 1.0)
    w
let zAtoms = trZ |> Array.map int64

// ---------------------------------------------------------------------------
// Mini-batching. The language has no rand.shuffle, so the epoch schedule is a
// table: row `ep` is a Fisher-Yates permutation of 0 .. nTrain-1 drawn from a
// fixed seed. The notebook's loss takes (epoch, batch) as trailing Int
// primals and reads this table -- the ONLY integer gather in the descent.
// ---------------------------------------------------------------------------
let nEpoch = 8
let seedPerm = 20260826
let perm =
    let rng = Random(seedPerm)
    let p = Array.zeroCreate<int64> (nEpoch * nTrain)
    for ep in 0 .. nEpoch - 1 do
        let a = Array.init nTrain id
        for i in nTrain - 1 .. -1 .. 1 do
            let j = rng.Next(i + 1)
            let t = a.[i] in a.[i] <- a.[j]; a.[j] <- t
        for i in 0 .. nTrain - 1 do p.[ep * nTrain + i] <- int64 a.[i]
    p

// ---------------------------------------------------------------------------
// Verification rotations: 8 Haar samples, row-major 3x3, from the ML oracle's
// own sampler. The notebook rotates test coordinates by these and checks that
// the predicted energy does not move and the predicted force vector does.
// ---------------------------------------------------------------------------
let nRot = 8
let seedRot = 8082026
let rot3 =
    let rng = Random(seedRot)
    let out = Array.zeroCreate<float> (nRot * 9)
    for k in 0 .. nRot - 1 do
        let r = Rotations.randomRotation rng
        for i in 0 .. 2 do
            for j in 0 .. 2 do
                out.[k * 9 + i * 3 + j] <- r.[i].[j]
    out

/// Orthogonality residual of the sampled rotations -- the store's own claim
/// that these really are rotations, checked before anything relies on it.
let rotResidual =
    [| for k in 0 .. nRot - 1 do
         for i in 0 .. 2 do
           for j in 0 .. 2 ->
             let mutable s = 0.0
             for c in 0 .. 2 do s <- s + rot3.[k * 9 + i * 3 + c] * rot3.[k * 9 + j * 3 + c]
             abs (s - (if i = j then 1.0 else 0.0)) |]
    |> Array.max
if rotResidual > 1e-12 then failwithf "sampled rotations are not orthogonal (%g)" rotResidual

// ---------------------------------------------------------------------------
// Write the store (idempotent: replaced wholesale).
// ---------------------------------------------------------------------------
let root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))
let store = Path.Combine(root, "examples", "data", "aspirin.zarr")
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

// e_stats slots: 0 = train-subset energy mean (kcal/mol), 1 = its standard
// deviation (kcal/mol), 2 = the radial width sigma (angstrom). The notebook
// multiplies a standardized MAE by slot 1 to report kcal/mol.
let eStats = [| eMean; eStd; sigma |]

ZarrWrite.writeStoreV3 store
    [ fvar "pos_train" [ "samp_tr"; "atom"; "xyz" ] [ nTrain; nAtom; 3 ] posTrain
      fvar "pos_test" [ "samp_te"; "atom"; "xyz" ] [ nTest; nAtom; 3 ] posTest
      fvar "pos_train_yzx" [ "samp_tr"; "atom"; "xyz" ] [ nTrain; nAtom; 3 ] posTrainYzx
      fvar "pos_test_yzx" [ "samp_te"; "atom"; "xyz" ] [ nTest; nAtom; 3 ] posTestYzx
      fvar "e_train" [ "samp_tr" ] [ nTrain ] eTrain
      fvar "e_test" [ "samp_te" ] [ nTest ] eTest
      fvar "e_stats" [ "stat" ] [ 3 ] eStats
      fvar "f_test" [ "samp_te"; "atom"; "xyz" ] [ nTest; nAtom; 3 ] fTest
      fvar "f_train" [ "samp_tr"; "atom"; "xyz" ] [ nTrain; nAtom; 3 ] fTrain
      fvar "spec_w" [ "atom"; "species" ] [ nAtom; 3 ] specW
      ivar "z_atoms" [ "atom" ] [ nAtom ] zAtoms
      ivar "perm" [ "epoch"; "samp_tr" ] [ nEpoch; nTrain ] perm
      fvar "rot3" [ "rotk"; "nine" ] [ nRot; 9 ] rot3 ]

// ---------------------------------------------------------------------------
// The pins the notebook relies on, and the decoder proof.
// ---------------------------------------------------------------------------
let fmt (a: float[]) = String.Join(", ", a |> Array.map (sprintf "%.17g"))

printfn "wrote %s" store
printfn "  molecule: aspirin C%d O%d H%d, %d atoms; z = %s"
    nC nO nH nAtom (String.Join(" ", trZ |> Array.map string))
printfn "  raw split: train %d, test %d; subset: every %dth / every %dth frame (%d + %d)"
    nRawTrain nRawTest strideTrain strideTest nTrain nTest
printfn ""
printfn "  FORTRAN-ORDER PROOF (train_R, descr=%s fortran_order=%b shape=%A)"
    hTrR.Descr hTrR.Fortran hTrR.Shape
printfn "    R[0,0,:] via the generic reader        = %s" (fmt genericAtom0)
printfn "    R[0,0,:] hand-decoded, column-major    = %s" (fmt handAtom0)
printfn "    R[0,0,:] hand-decoded, C-major (WRONG) = %s" (fmt wrongAtom0)
printfn "    reader-vs-hand residual = %g   (0 required)" atom0Residual
printfn "    nearest-neighbour band over sample 0's 21 atoms, and how far those"
printfn "    same distances drift across the first 50 samples:"
printfn "      column-major : nn %.4f .. %.4f A, drift %.4f A  <- covalent bonds"
    nnMinF nnMaxF spreadF
printfn "      C-major      : nn %.4f .. %.4f A, drift %.4f A  <- not a molecule"
    nnMinC nnMaxC spreadC
printfn ""
printfn "  e_stats = [mean %.6f, std %.6f] kcal/mol, sigma = %.6f A" eMean eStd sigma
printfn "  standardized test energies: mean %.6f, mean-square %.6f (both near 0 and 1 iff the
    two subsets sample the same distribution)" (Array.average eTest) (eTest |> Array.averageBy (fun v -> v * v))
printfn "  e_train[0..3] (standardized) = %s" (fmt (Array.sub eTrain 0 4))
printfn "  e_test[0..3]  (standardized) = %s" (fmt (Array.sub eTest 0 4))
printfn "  pos_train[0,0,:] = %s" (fmt (Array.sub posTrain 0 3))
printfn "  pos_test[0,0,:]  = %s" (fmt (Array.sub posTest 0 3))
printfn "  pos_test_yzx[0,0,:] = %s" (fmt (Array.sub posTestYzx 0 3))
printfn "  f_test[0,0,:] = %s  (kcal/mol/A, raw)" (fmt (Array.sub fTest 0 3))
printfn "  spec_w row order (C,O,H); per-species counts = %d %d %d" nC nO nH
printfn "  perm: %d epochs x %d, Fisher-Yates, seed %d; perm[0,0..7] = %s"
    nEpoch nTrain seedPerm
    (String.Join(", ", Array.sub perm 0 8 |> Array.map string))
printfn "  rot3: %d Haar rotations, seed %d, orthogonality residual %g"
    nRot seedRot rotResidual
printfn "  rot3[0] = %s" (fmt (Array.sub rot3 0 9))
let sumsq (a: float[]) = a |> Array.sumBy (fun v -> v * v)
printfn "  sum(pos_train^2) = %.17g   sum(pos_test^2) = %.17g"
    (sumsq posTrain) (sumsq posTest)
printfn "  sum(f_test^2) = %.17g" (sumsq fTest)
