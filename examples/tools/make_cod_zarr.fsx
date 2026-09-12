// make_cod_zarr.fsx — real crystal structures from the Crystallography Open
// Database (COD, CC0) turned into neighbour-direction point clouds for the
// bond-orientational-order notebook (examples/cod_crystals.bladenb).
//
// Input: a local mirror of individual COD CIF files under
//   C:\Users\cdupu\Data\cod\cifs\<codid>.cif
// fetched (serially, politely) from https://www.crystallography.net/cod/<id>.cif
// against the id lists returned by the verified COD REST endpoint
//   https://www.crystallography.net/cod/result?space_group_number=<N>&format=json
// for the five CUBIC target space groups:
//   Fm-3m (225)  Im-3m (229)  Pm-3m (221)  Fd-3m (227)  F-43m (216)
//
// Cubic is a deliberate scientific AND engineering restriction: a = b = c and
// alpha = beta = gamma = 90, so fractional -> Cartesian is a single scale
// factor (no metric tensor), and cubic site symmetry is exactly what forces
// every first-shell rank-2 tensor isotropic — the K <= 2 blindness the
// notebook pins is a theorem about the crystals, not preprocessing.
//
// Pipeline, per entry (every reject is COUNTED and printed — the yield is the
// story):
//   1. parse the CIF (BCL-only F#: cell, space group, the explicit symmetry-
//      operator loop, the atom-site loop; uncertainties like 3.6150(2)
//      stripped). Entries with missing/implicit symmetry operators are
//      SKIPPED, never reconstructed from a hardcoded space-group table.
//   2. skip disorder: any site occupancy < 1, or two expanded sites closer
//      than 0.5 A.
//   3. expand every operator over every site, wrap into [0,1), dedupe, build
//      a 3x3x3 Cartesian supercell (scale = a).
//   4. for each symmetry-inequivalent site: sort neighbour distances, take
//      the first shell by a distance-GAP criterion (next distance >= 1.08x
//      the shell edge), require the shell CLEAN (all distances within 1% of
//      the shortest) and coordination number in {4, 6, 8, 12}; emit unit
//      direction vectors.
//   5. label by GEOMETRY, not by the space group: the shell's own l=3 and
//      l=4 rotation-invariant powers (computed here via real spherical
//      harmonics, calibrated below against the probe-verified canonical
//      table) must match one canonical shell class within a tight tolerance;
//      anything else is rejected and counted. The label is definitional-but-
//      verified, and the store is self-validating.
//
// The invariant powers used everywhere: with qbar_lm = mean_p Y_lm(v_p) over
// the shell's unit vectors and Ql^2 = 4pi/(2l+1) sum_m qbar_lm^2 (Steinhardt),
// the power of the l-block of the K-th derive_poly jet of the same vectors is
//   power_l = c_l * Ql^2,   c_l = 2^l (l!)^2 / (2l)!   (c2=2/3, c3=2/5, c4=8/35)
// — verified below by an assertion gate that reproduces, to 1e-6, the probe
// table:
//   shell            CN   K3 power        K4 l=4 power
//   simple cubic      6   0               2/15    = 0.133333
//   fcc               12  0               1/120   = 0.008333
//   bcc               8   0               8/135   = 0.059259
//   hcp               12  1/432 = 0.00231 7/3240  = 0.002160
//   tetrahedral       4   2/9   = 0.22222 8/135   = 0.059259
//   icosahedral       12  0               0
// and every K=2 l=2 power is machine zero. bcc and tetrahedral are DEGENERATE
// at l=4 (a cube is a tetrahedron plus its inverse; l=4 is even) and separate
// only through the odd K=3 channel.
//
// Store classes (the four reachable from cubic space groups): 0 = sc
// (octahedral CN6), 1 = fcc (cuboctahedral CN12), 2 = bcc (cubic CN8),
// 3 = tet (tetrahedral CN4). Shells matching hcp/icosahedral geometry are
// counted separately and excluded from the store.
//
// Per stored sample: unit vectors zero-padded to a fixed 12 (a zero vector
// contributes exactly zero to every K >= 1 moment — derive_poly is
// homogeneous of degree K), in BOTH (x,y,z) and the real-l=1 (y,z,x) order;
// true coordination number; class label + one-hot; the source COD id and the
// site ordinal (full provenance); a per-sample Haar rotation ALREADY APPLIED
// (seeded); a 75/25 train/test split; plus 8 fresh baked test rotations and
// the sample's own l=2/l=3/l=4 powers computed here (the .blade validation
// recomputes them through ml.derive_poly — an F#-vs-compiler differential).
//
// Run from anywhere:  dotnet fsi examples/tools/make_cod_zarr.fsx
// Idempotent: the CIF mirror is the only input, the file list is sorted
// numerically, every RNG is seeded; a second run rewrites identical bytes
// (the store content hash is printed; two runs must agree).

#r "System.Security.Cryptography"
// The prefix of Blade.fsproj's compile order that IR.fs and ZarrProvider need,
// in that order — fsi has no project file, so the dependency chain is spelled
// out here, exactly as make_moments_zarr.fsx does.
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
open System.Globalization
open System.IO
open System.Security.Cryptography
open Blade.ZarrProvider
open BladeML

// ---------------------------------------------------------------------------
// Knobs, seeds, tolerances.
// ---------------------------------------------------------------------------
let cifDir = @"C:\Users\cdupu\Data\cod\cifs"
let targetSgs = set [ 216; 221; 225; 227; 229 ]
let nPad = 12               // fixed point count; shells zero-padded up to it
let capPerClass = 400
let minPerClassWarn = 150
let nRot = 8                // fresh baked verification rotations

let seedRotTrain = 20260827
let seedRotTest = 31415926
let seedRotVerify = 27182818

let cellEqTol = 1e-3        // |a-b|/a for the cubic re-check
let angleTol = 0.02         // degrees off 90
let occTol = 0.999          // any site below this = disorder, reject entry
let orbitDedupTol = 1e-3    // fractional, within one site's symmetry orbit
let collideTolA = 0.5       // Angstrom: distinct sites closer = split-site
let cellMinA, cellMaxA = 2.0, 40.0
let maxCellAtoms = 1500
let shellCleanTol = 0.01    // (d_max - d_min)/d_min within the first shell
let shellGapMin = 0.08      // d_next >= d_shellmax * (1 + this)
let labelTol = 0.003        // |l3 - target| and |l4 - target| for the match
let l2Gate = 1e-6           // accepted shells must have l=2 power below this

// ---------------------------------------------------------------------------
// Minimal CIF reader: scalar tags + loops, quoted tokens, ;-text blocks,
// uncertainty stripping. Robust-subset by design — anything it cannot read
// becomes a counted reject, never a guess.
// ---------------------------------------------------------------------------
type Cif =
    { Tags: Map<string, string>
      Loops: (string list * string[] list) list }

let tokenizeLine (line: string) =
    let toks = ResizeArray<string>()
    let mutable i = 0
    let n = line.Length
    while i < n do
        while i < n && (line.[i] = ' ' || line.[i] = '\t') do i <- i + 1
        if i < n then
            if line.[i] = '#' then i <- n           // comment to end of line
            elif line.[i] = '\'' || line.[i] = '"' then
                let q = line.[i]
                let s = i + 1
                let mutable e = s
                while e < n && line.[e] <> q do e <- e + 1
                toks.Add(line.Substring(s, e - s))
                i <- e + 1
            else
                let s = i
                while i < n && line.[i] <> ' ' && line.[i] <> '\t' do i <- i + 1
                toks.Add(line.Substring(s, i - s))
    List.ofSeq toks

/// Parse one CIF file (first data_ block only).
let parseCif (path: string) : Cif =
    let rawLines = File.ReadAllLines path
    // Fold ;-delimited text blocks into single tokens; drop their content
    // (only used for prose fields we never read).
    let tokens = ResizeArray<string>()
    let mutable inText = false
    let mutable dataBlocks = 0
    let mutable stop = false
    for line in rawLines do
        if not stop then
            if inText then
                if line.StartsWith ";" then
                    inText <- false
                    tokens.Add "\x01TEXT"           // placeholder value token
            elif line.StartsWith ";" then
                inText <- true
            else
                for t in tokenizeLine line do
                    if not stop then
                        if t.StartsWith "data_" then
                            dataBlocks <- dataBlocks + 1
                            if dataBlocks > 1 then stop <- true
                        elif dataBlocks >= 1 then tokens.Add t
                        else ()                     // preamble before data_
    // Walk the token stream.
    let mutable tags = Map.empty
    let loops = ResizeArray<string list * string[] list>()
    let mutable i = 0
    let n = tokens.Count
    while i < n do
        let t = tokens.[i]
        if t = "loop_" then
            i <- i + 1
            let hdr = ResizeArray<string>()
            while i < n && tokens.[i].StartsWith "_" do
                hdr.Add(tokens.[i].ToLowerInvariant())
                i <- i + 1
            let w = hdr.Count
            let rows = ResizeArray<string[]>()
            let cur = ResizeArray<string>()
            let mutable go = w > 0
            while go && i < n do
                let v = tokens.[i]
                if v.StartsWith "_" || v = "loop_" then go <- false
                else
                    cur.Add v
                    if cur.Count = w then
                        rows.Add(cur.ToArray())
                        cur.Clear()
                    i <- i + 1
            if w > 0 then loops.Add(List.ofSeq hdr, List.ofSeq rows)
        elif t.StartsWith "_" then
            let key = t.ToLowerInvariant()
            if i + 1 < n && not (tokens.[i + 1].StartsWith "_") && tokens.[i + 1] <> "loop_" then
                tags <- Map.add key tokens.[i + 1] tags
                i <- i + 2
            else
                tags <- Map.add key "" tags
                i <- i + 1
        else i <- i + 1
    { Tags = tags; Loops = List.ofSeq loops }

/// Strip a trailing standard-uncertainty "(12)" and parse invariant-culture.
let parseNum (s: string) : float option =
    if s = "." || s = "?" || s = "" then None
    else
        let core =
            let p = s.IndexOf '('
            if p >= 0 then s.Substring(0, p) else s
        match Double.TryParse(core, NumberStyles.Float, CultureInfo.InvariantCulture) with
        | true, v -> Some v
        | _ -> None

let tag (cif: Cif) (names: string list) =
    names |> List.tryPick (fun k -> Map.tryFind k cif.Tags)

let findLoopCol (cif: Cif) (colNames: string list) =
    cif.Loops
    |> List.tryPick (fun (hdr, rows) ->
        colNames
        |> List.tryPick (fun c ->
            hdr |> List.tryFindIndex ((=) c) |> Option.map (fun ix -> (hdr, rows, ix))))

// ---------------------------------------------------------------------------
// Symmetry-operator parsing: "1/2+x, y, -z" -> (3x3 matrix, translation).
// ---------------------------------------------------------------------------
let parseSymTerm (row: float[]) (t: float byref) (term: string) =
    // term: like "x", "-y", "+1/2", "0.25", "2x" (never seen in cubic, but
    // a leading integer coefficient is honored).
    let s = term.Trim()
    if s = "" then true
    else
        let sign, body =
            if s.[0] = '-' then -1.0, s.Substring 1
            elif s.[0] = '+' then 1.0, s.Substring 1
            else 1.0, s
        let axis = body |> Seq.tryFindIndex (fun c -> c = 'x' || c = 'y' || c = 'z')
        match axis with
        | Some ai ->
            let c = body.[ai]
            let coefStr = body.Remove(ai, 1)
            let coef =
                if coefStr = "" then 1.0
                elif coefStr.Contains "/" then
                    let ps = coefStr.Split '/'
                    match parseNum ps.[0], parseNum ps.[1] with
                    | Some a, Some b when b <> 0.0 -> a / b
                    | _ -> nan
                else match parseNum coefStr with Some v -> v | None -> nan
            if Double.IsNaN coef then false
            else
                let k = (int c) - (int 'x')
                row.[k] <- row.[k] + sign * coef
                true
        | None ->
            let v =
                if body.Contains "/" then
                    let ps = body.Split '/'
                    match parseNum ps.[0], parseNum ps.[1] with
                    | Some a, Some b when b <> 0.0 -> Some(a / b)
                    | _ -> None
                else parseNum body
            match v with
            | Some x -> t <- t + sign * x; true
            | None -> false

let parseSymOp (s: string) : (float[][] * float[]) option =
    let comps = s.ToLowerInvariant().Replace(" ", "").Split ','
    if comps.Length <> 3 then None
    else
        let m = Array.init 3 (fun _ -> Array.zeroCreate<float> 3)
        let tr = Array.zeroCreate<float> 3
        let mutable ok = true
        for ci in 0 .. 2 do
            // split the component into signed terms
            let comp = comps.[ci]
            let terms = ResizeArray<string>()
            let mutable start = 0
            for i in 1 .. comp.Length - 1 do
                if (comp.[i] = '+' || comp.[i] = '-') && comp.[i - 1] <> '/' then
                    terms.Add(comp.Substring(start, i - start))
                    start <- i
            terms.Add(comp.Substring start)
            let mutable t = tr.[ci]
            for term in terms do
                if not (parseSymTerm m.[ci] &t term) then ok <- false
            tr.[ci] <- t
        if ok then Some(m, tr) else None

// ---------------------------------------------------------------------------
// Real spherical harmonics (Cartesian tesseral forms, |v| = 1) for l = 2,3,4,
// the Steinhardt Ql, and the derive_poly l-block powers.
// ---------------------------------------------------------------------------
let pi = Math.PI

let y2 : (float -> float -> float -> float)[] =
    [| fun x y _ -> 0.5 * sqrt (15.0 / pi) * x * y
       fun _ y z -> 0.5 * sqrt (15.0 / pi) * y * z
       fun x y z -> 0.25 * sqrt (5.0 / pi) * (3.0 * z * z - 1.0)
       fun x _ z -> 0.5 * sqrt (15.0 / pi) * x * z
       fun x y _ -> 0.25 * sqrt (15.0 / pi) * (x * x - y * y) |]

let y3 : (float -> float -> float -> float)[] =
    [| fun x y _ -> 0.25 * sqrt (35.0 / (2.0 * pi)) * y * (3.0 * x * x - y * y)
       fun x y z -> 0.5 * sqrt (105.0 / pi) * x * y * z
       fun _ y z -> 0.25 * sqrt (21.0 / (2.0 * pi)) * y * (5.0 * z * z - 1.0)
       fun _ _ z -> 0.25 * sqrt (7.0 / pi) * z * (5.0 * z * z - 3.0)
       fun x _ z -> 0.25 * sqrt (21.0 / (2.0 * pi)) * x * (5.0 * z * z - 1.0)
       fun x y z -> 0.25 * sqrt (105.0 / pi) * z * (x * x - y * y)
       fun x y _ -> 0.25 * sqrt (35.0 / (2.0 * pi)) * x * (x * x - 3.0 * y * y) |]

let y4 : (float -> float -> float -> float)[] =
    [| fun x y _ -> 0.75 * sqrt (35.0 / pi) * x * y * (x * x - y * y)
       fun x y z -> 0.75 * sqrt (35.0 / (2.0 * pi)) * y * z * (3.0 * x * x - y * y)
       fun x y z -> 0.75 * sqrt (5.0 / pi) * x * y * (7.0 * z * z - 1.0)
       fun _ y z -> 0.75 * sqrt (5.0 / (2.0 * pi)) * y * z * (7.0 * z * z - 3.0)
       fun _ _ z -> (3.0 / 16.0) * sqrt (1.0 / pi) * (35.0 * z * z * z * z - 30.0 * z * z + 3.0)
       fun x _ z -> 0.75 * sqrt (5.0 / (2.0 * pi)) * x * z * (7.0 * z * z - 3.0)
       fun x y z -> (3.0 / 8.0) * sqrt (5.0 / pi) * (x * x - y * y) * (7.0 * z * z - 1.0)
       fun x y z -> 0.75 * sqrt (35.0 / (2.0 * pi)) * x * z * (x * x - 3.0 * y * y)
       fun x y _ -> (3.0 / 16.0) * sqrt (35.0 / pi) * (x * x * x * x - 6.0 * x * x * y * y + y * y * y * y) |]

/// Ql^2 = 4pi/(2l+1) sum_m (mean_p Y_lm)^2 over unit vectors.
let ql2 (ys: (float -> float -> float -> float)[]) (l: int) (vs: float[][]) =
    let n = float vs.Length
    let s =
        ys
        |> Array.sumBy (fun f ->
            let q = (vs |> Array.sumBy (fun v -> f v.[0] v.[1] v.[2])) / n
            q * q)
    4.0 * pi / float (2 * l + 1) * s

// c_l = 2^l (l!)^2 / (2l)!  : the exact derive_poly-power / Ql^2 ratio.
let c2, c3, c4 = 2.0 / 3.0, 2.0 / 5.0, 8.0 / 35.0
let powers (vs: float[][]) =
    c2 * ql2 y2 2 vs, c3 * ql2 y3 3 vs, c4 * ql2 y4 4 vs

// ---------------------------------------------------------------------------
// Calibration gate: the canonical shells must reproduce the target table
// above before any real data is touched.
// ---------------------------------------------------------------------------
let canonSc =
    [| [| 1.;0.;0. |]; [| -1.;0.;0. |]; [| 0.;1.;0. |]; [| 0.;-1.;0. |]; [| 0.;0.;1. |]; [| 0.;0.;-1. |] |]
let canonBcc =
    let s = 1.0 / sqrt 3.0
    [| for sx in [1.;-1.] do for sy in [1.;-1.] do for sz in [1.;-1.] -> [| sx*s; sy*s; sz*s |] |]
let canonTet =
    let s = 1.0 / sqrt 3.0
    [| [| s;s;s |]; [| s;-s;-s |]; [| -s;s;-s |]; [| -s;-s;s |] |]
let canonFcc =
    let s = 1.0 / sqrt 2.0
    [| [| s;s;0. |]; [| s;-s;0. |]; [| -s;s;0. |]; [| -s;-s;0. |]
       [| s;0.;s |]; [| s;0.;-s |]; [| -s;0.;s |]; [| -s;0.;-s |]
       [| 0.;s;s |]; [| 0.;s;-s |]; [| 0.;-s;s |]; [| 0.;-s;-s |] |]
let canonHcp =
    // ideal c/a: 6 in-plane hexagon + 3 up + 3 down ECLIPSED (anticuboctahedron)
    let h = sqrt 3.0 / 2.0
    let up = [| [| 0.0; 1.0/sqrt 3.0; sqrt (2.0/3.0) |]
                [| 0.5; -0.5/sqrt 3.0; sqrt (2.0/3.0) |]
                [| -0.5; -0.5/sqrt 3.0; sqrt (2.0/3.0) |] |]
    [| [| 1.;0.;0. |]; [| -1.;0.;0. |]; [| 0.5; h; 0. |]; [| -0.5; h; 0. |]
       [| 0.5; -h; 0. |]; [| -0.5; -h; 0. |]
       yield! up
       yield! up |> Array.map (fun v -> [| v.[0]; v.[1]; -v.[2] |]) |]
let canonIco =
    let phi = (1.0 + sqrt 5.0) / 2.0
    let nrm = sqrt (1.0 + phi * phi)
    [| for perm in 0 .. 2 do
         for s1 in [ 1.0; -1.0 ] do
           for s2 in [ 1.0; -1.0 ] do
             let v = [| 0.0; s1 / nrm; s2 * phi / nrm |]
             yield Array.init 3 (fun i -> v.[(i + 3 - perm) % 3]) |]

// (name, CN, l3 target, l4 target); the first four are the store classes.
let classTargets =
    [| "sc",  6,  0.0,        2.0 / 15.0
       "fcc", 12, 0.0,        1.0 / 120.0
       "bcc", 8,  0.0,        8.0 / 135.0
       "tet", 4,  2.0 / 9.0,  8.0 / 135.0
       "hcp", 12, 1.0 / 432.0, 7.0 / 3240.0
       "ico", 12, 0.0,        0.0 |]
let nClass = 4

let calibrate () =
    let cases =
        [ canonSc, "sc"; canonFcc, "fcc"; canonBcc, "bcc"
          canonTet, "tet"; canonHcp, "hcp"; canonIco, "ico" ]
    for vs, name in cases do
        let p2, p3, p4 = powers vs
        let (_, _, t3, t4) = classTargets |> Array.find (fun (n, _, _, _) -> n = name)
        if abs p2 > 1e-12 then failwithf "calibration: %s l2 power %g not zero" name p2
        if abs (p3 - t3) > 1e-6 then failwithf "calibration: %s l3 power %.9f vs %.9f" name p3 t3
        if abs (p4 - t4) > 1e-6 then failwithf "calibration: %s l4 power %.9f vs %.9f" name p4 t4
        printfn "  calib %-3s CN%-2d l2 %.2e  l3 %.8f  l4 %.8f  (targets %.8f %.8f)"
            name vs.Length p2 p3 p4 t3 t4
printfn "spherical-harmonic calibration against the probe table:"
calibrate ()

// ---------------------------------------------------------------------------
// Per-entry extraction.
// ---------------------------------------------------------------------------
type Sample =
    { CodId: int64
      SgNum: int
      SiteOrdinal: int
      SiteLabel: string
      Cn: int
      Cls: int              // index into classTargets (0..3 stored)
      Dirs: float[][]       // CN unit vectors, (x,y,z)
      P2: float; P3: float; P4: float
      ShellSpread: float }  // (dmax-dmin)/dmin within the shell

let rejects = System.Collections.Generic.SortedDictionary<string, int>()
let bump (k: string) =
    rejects.[k] <- (match rejects.TryGetValue k with | true, v -> v | _ -> 0) + 1

let wrap01 (f: float) = f - floor f

let extractEntry (codId: int64) (cif: Cif) : Sample list =
    // -- cell --
    let num names = tag cif names |> Option.bind parseNum
    let a = num [ "_cell_length_a" ]
    let b = num [ "_cell_length_b" ]
    let c = num [ "_cell_length_c" ]
    let al = num [ "_cell_angle_alpha" ]
    let be = num [ "_cell_angle_beta" ]
    let ga = num [ "_cell_angle_gamma" ]
    match a, b, c with
    | Some a, Some b, Some c when a > 0.0 ->
        let angOk =
            [ al; be; ga ]
            |> List.forall (fun x -> match x with Some v -> abs (v - 90.0) <= angleTol | None -> false)
        if abs (a - b) > cellEqTol * a || abs (a - c) > cellEqTol * a || not angOk then
            bump "entry: non-cubic cell"; []
        elif a < cellMinA || a > cellMaxA then
            bump "entry: cell size out of range"; []
        else
        // -- space group --
        let sgNum =
            tag cif [ "_space_group_it_number"; "_symmetry_int_tables_number" ]
            |> Option.bind parseNum |> Option.map int
        match sgNum with
        | None -> bump "entry: no space-group number"; []
        | Some sg when not (targetSgs.Contains sg) ->
            bump "entry: space group not in target set"; []
        | Some sg ->
        // -- symmetry operators (explicit only; never reconstructed) --
        let opsLoop =
            findLoopCol cif [ "_symmetry_equiv_pos_as_xyz"; "_space_group_symop_operation_xyz" ]
        let ops =
            match opsLoop with
            | None ->
                match tag cif [ "_symmetry_equiv_pos_as_xyz"; "_space_group_symop_operation_xyz" ] with
                | Some s -> (match parseSymOp s with Some op -> [ op ] | None -> [])
                | None -> []
            | Some (_, rows, ix) ->
                rows |> List.choose (fun r -> parseSymOp r.[ix])
        if List.length ops < 2 then
            // a cubic group has >= 12 operators; 0/1 means implicit symmetry
            bump "entry: missing/implicit symmetry operators"; []
        else
        // -- atom sites --
        match findLoopCol cif [ "_atom_site_fract_x" ] with
        | None -> bump "entry: no atom-site loop"; []
        | Some (hdr, rows, ixX) ->
            let col name = hdr |> List.tryFindIndex ((=) name)
            let ixY = col "_atom_site_fract_y"
            let ixZ = col "_atom_site_fract_z"
            let ixOcc = col "_atom_site_occupancy"
            let ixLab =
                match col "_atom_site_label" with
                | Some i -> Some i
                | None -> col "_atom_site_type_symbol"
            match ixY, ixZ with
            | Some ixY, Some ixZ ->
                let sites =
                    rows
                    |> List.map (fun r ->
                        let fx = parseNum r.[ixX]
                        let fy = parseNum r.[ixY]
                        let fz = parseNum r.[ixZ]
                        let occ =
                            match ixOcc with
                            | Some i -> parseNum r.[i] |> Option.defaultValue 1.0
                            | None -> 1.0
                        let lab = match ixLab with Some i -> r.[i] | None -> "?"
                        fx, fy, fz, occ, lab)
                if sites |> List.exists (fun (fx, fy, fz, _, _) ->
                        fx.IsNone || fy.IsNone || fz.IsNone) then
                    bump "entry: unparseable coordinates"; []
                elif sites |> List.exists (fun (_, _, _, occ, _) -> occ < occTol) then
                    bump "entry: partial occupancy (disorder)"; []
                else
                let siteFracs =
                    sites
                    |> List.map (fun (fx, fy, fz, _, lab) ->
                        [| wrap01 fx.Value; wrap01 fy.Value; wrap01 fz.Value |], lab)
                // -- expand operators over sites, dedupe within each orbit --
                let orbitOf (p: float[]) =
                    let out = ResizeArray<float[]>()
                    for (m, t) in ops do
                        let q =
                            Array.init 3 (fun i ->
                                wrap01 (m.[i].[0] * p.[0] + m.[i].[1] * p.[1] + m.[i].[2] * p.[2] + t.[i]))
                        let dup =
                            out |> Seq.exists (fun r ->
                                let d k =
                                    let x = abs (r.[k] - q.[k])
                                    min x (1.0 - x)
                                d 0 <= orbitDedupTol && d 1 <= orbitDedupTol && d 2 <= orbitDedupTol)
                        if not dup then out.Add q
                    out
                let orbits = siteFracs |> List.map (fun (p, _) -> orbitOf p)
                let cellAtoms =
                    [| for si, orb in List.indexed orbits do
                         for p in orb -> si, p |]
                if cellAtoms.Length > maxCellAtoms then
                    bump "entry: expanded cell too large"; []
                else
                // -- split-site collision check (periodic min-image, Angstrom) --
                let minImageDist (p: float[]) (q: float[]) =
                    let d k =
                        let x = abs (p.[k] - q.[k])
                        let x = min x (1.0 - x)
                        x * a
                    sqrt (d 0 ** 2.0 + d 1 ** 2.0 + d 2 ** 2.0)
                let collision =
                    seq {
                        for i in 0 .. cellAtoms.Length - 1 do
                            for j in i + 1 .. cellAtoms.Length - 1 do
                                let si, p = cellAtoms.[i]
                                let sj, q = cellAtoms.[j]
                                if si <> sj then yield minImageDist p q }
                    |> Seq.exists (fun d -> d < collideTolA)
                if collision then bump "entry: split sites closer than 0.5 A"; []
                else
                bump "entry: accepted (cell parsed, ordered, expanded)"
                // -- 3x3x3 supercell in Cartesian (scale = a) --
                let super =
                    [| for _, p in cellAtoms do
                         for dx in -1.0 .. 1.0 do
                           for dy in -1.0 .. 1.0 do
                             for dz in -1.0 .. 1.0 ->
                               [| (p.[0] + dx) * a; (p.[1] + dy) * a; (p.[2] + dz) * a |] |]
                // -- per symmetry-inequivalent site: first shell --
                siteFracs
                |> List.mapi (fun si (p, lab) ->
                    let ctr = [| p.[0] * a; p.[1] * a; p.[2] * a |]
                    let ds =
                        super
                        |> Array.choose (fun q ->
                            let dx = q.[0] - ctr.[0]
                            let dy = q.[1] - ctr.[1]
                            let dz = q.[2] - ctr.[2]
                            let d = sqrt (dx * dx + dy * dy + dz * dz)
                            if d > 1e-6 then Some(d, [| dx; dy; dz |]) else None)
                        |> Array.sortBy fst
                    if ds.Length < 5 then bump "site: too few neighbours"; None
                    else
                        let d1 = fst ds.[0]
                        // shell = prefix within 3% of d1 (segmentation), then
                        // cleanliness and gap are checked strictly
                        let m =
                            let mutable k = 1
                            while k < ds.Length && fst ds.[k] <= d1 * 1.03 do k <- k + 1
                            k
                        let dmax = fst ds.[m - 1]
                        if m >= ds.Length then bump "site: shell fills cutoff"; None
                        elif fst ds.[m] < dmax * (1.0 + shellGapMin) then
                            bump "site: no clear distance gap"; None
                        elif (dmax - d1) / d1 > shellCleanTol then
                            bump "site: dirty shell (spread > 1%)"; None
                        elif m > nPad then bump "site: CN > 12"; None
                        elif not (List.contains m [ 4; 6; 8; 12 ]) then
                            bump (sprintf "site: CN not in {4,6,8,12} (CN=%d)" m); None
                        else
                            let dirs =
                                ds.[0 .. m - 1]
                                |> Array.map (fun (d, v) -> Array.map (fun x -> x / d) v)
                            let p2, p3, p4 = powers dirs
                            // geometry label: nearest CN-compatible canonical class
                            let cands =
                                classTargets
                                |> Array.indexed
                                |> Array.filter (fun (_, (_, cn, _, _)) -> cn = m)
                            if cands.Length = 0 then bump "site: no class at this CN"; None
                            else
                                let ci, (nm, _, t3, t4) =
                                    cands
                                    |> Array.minBy (fun (_, (_, _, t3, t4)) ->
                                        (p3 - t3) ** 2.0 + (p4 - t4) ** 2.0)
                                if abs (p3 - t3) > labelTol || abs (p4 - t4) > labelTol then
                                    bump "site: geometry matches no canonical class"; None
                                elif abs p2 > l2Gate then
                                    bump "site: l2 power above gate (distorted)"; None
                                elif ci >= nClass then
                                    bump (sprintf "site: matched off-target class %s" nm); None
                                else
                                    Some { CodId = codId; SgNum = sg; SiteOrdinal = si
                                           SiteLabel = lab; Cn = m; Cls = ci; Dirs = dirs
                                           P2 = p2; P3 = p3; P4 = p4
                                           ShellSpread = (dmax - d1) / d1 })
                |> List.choose id
            | _ -> bump "entry: no atom-site loop"; []
    | _ -> bump "entry: no cell lengths"; []

// ---------------------------------------------------------------------------
// Sweep the mirror (sorted numerically: determinism does not depend on
// filesystem enumeration order).
// ---------------------------------------------------------------------------
let files =
    Directory.GetFiles(cifDir, "*.cif")
    |> Array.sortBy (fun f -> Int64.Parse(Path.GetFileNameWithoutExtension f))
printfn "CIF mirror: %d files under %s" files.Length cifDir

let samples =
    files
    |> Array.collect (fun f ->
        let codId = Int64.Parse(Path.GetFileNameWithoutExtension f)
        try extractEntry codId (parseCif f) |> Array.ofList
        with _ -> bump "entry: parse exception"; [||])

printfn "reject census (entries -> sites):"
for kv in rejects do printfn "  %6d  %s" kv.Value kv.Key
printfn "candidate samples: %d" samples.Length

// ---------------------------------------------------------------------------
// Per-class cap (stratified across the id range) and 75/25 split.
// ---------------------------------------------------------------------------
let byClass =
    Array.init nClass (fun c ->
        samples
        |> Array.filter (fun s -> s.Cls = c)
        |> Array.sortBy (fun s -> s.CodId, s.SiteOrdinal))
let picked =
    byClass
    |> Array.map (fun arr ->
        if arr.Length <= capPerClass then arr
        else Array.init capPerClass (fun i -> arr.[i * arr.Length / capPerClass]))
for c in 0 .. nClass - 1 do
    let (nm, cn, _, _) = classTargets.[c]
    printfn "class %d %-3s (CN%-2d): %5d candidates -> %4d stored" c nm cn byClass.[c].Length picked.[c].Length
    if picked.[c].Length < minPerClassWarn then
        printfn "  WARNING: class %s below the %d-sample target — reported, not hidden" nm minPerClassWarn

// split: within each class, every 4th of the picked list is test.
let trainByClass = picked |> Array.map (Array.indexed >> Array.filter (fun (i, _) -> i % 4 <> 3) >> Array.map snd)
let testByClass = picked |> Array.map (Array.indexed >> Array.filter (fun (i, _) -> i % 4 = 3) >> Array.map snd)
let nTrain = trainByClass |> Array.sumBy Array.length
let nTest = testByClass |> Array.sumBy Array.length

// ---------------------------------------------------------------------------
// Haar rotations (seeded, class-major train then class-major test), padding,
// flattening, per-sample powers recomputed from the FINAL stored vectors so
// the .blade validation compares numbers computed from identical bytes.
// ---------------------------------------------------------------------------
let applyRot (r: float[][]) (v: float[]) =
    Array.init 3 (fun i -> r.[i].[0] * v.[0] + r.[i].[1] * v.[1] + r.[i].[2] * v.[2])

let flattenSplit (byC: Sample[][]) (rng: Random) =
    let all = byC |> Array.collect id
    let n = all.Length
    let pos = Array.zeroCreate<float> (n * nPad * 3)
    let posYzx = Array.zeroCreate<float> (n * nPad * 3)
    let lab = Array.zeroCreate<int64> n
    let cn = Array.zeroCreate<int64> n
    let cnf = Array.zeroCreate<float> n
    let cod = Array.zeroCreate<int64> n
    let site = Array.zeroCreate<int64> n
    let pow = Array.zeroCreate<float> (n * 3)
    let oneHot = Array.zeroCreate<float> (n * nClass)
    for s in 0 .. n - 1 do
        let smp = all.[s]
        let rot = Rotations.randomRotation rng
        let dirs = smp.Dirs |> Array.map (applyRot rot)
        lab.[s] <- int64 smp.Cls
        cn.[s] <- int64 smp.Cn
        cnf.[s] <- float smp.Cn
        cod.[s] <- smp.CodId
        site.[s] <- int64 smp.SiteOrdinal
        oneHot.[s * nClass + smp.Cls] <- 1.0
        for p in 0 .. smp.Cn - 1 do
            let b = (s * nPad + p) * 3
            pos.[b] <- dirs.[p].[0]; pos.[b + 1] <- dirs.[p].[1]; pos.[b + 2] <- dirs.[p].[2]
            posYzx.[b] <- dirs.[p].[1]; posYzx.[b + 1] <- dirs.[p].[2]; posYzx.[b + 2] <- dirs.[p].[0]
        let p2, p3, p4 = powers dirs
        pow.[s * 3] <- p2; pow.[s * 3 + 1] <- p3; pow.[s * 3 + 2] <- p4
    pos, posYzx, lab, cn, cnf, cod, site, pow, oneHot, n

let posTr, posTrY, labTr, cnTr, cnfTr, codTr, siteTr, powTr, yTr, nTr =
    flattenSplit trainByClass (Random seedRotTrain)
let posTe, posTeY, labTe, cnTe, cnfTe, codTe, siteTe, powTe, yTe, nTe =
    flattenSplit testByClass (Random seedRotTest)
if nTr <> nTrain || nTe <> nTest then failwith "split bookkeeping drifted"

// Fresh verification rotations, row-major 3x3.
let rot3 =
    let rng = Random seedRotVerify
    let out = Array.zeroCreate<float> (nRot * 9)
    for k in 0 .. nRot - 1 do
        let r = Rotations.randomRotation rng
        for i in 0 .. 2 do
            for j in 0 .. 2 do
                out.[k * 9 + i * 3 + j] <- r.[i].[j]
    out

// ---------------------------------------------------------------------------
// Per-class spread report on the stored samples: the "experimental
// distortion" the notebook will advertise, measured honestly.
// ---------------------------------------------------------------------------
let spreadReport (name: string) (byC: Sample[][]) =
    printfn "%s per-class l4-power spread (min / mean / max) and worst l2, shell spread:" name
    for c in 0 .. nClass - 1 do
        let arr = byC.[c]
        if arr.Length > 0 then
            let l4s = arr |> Array.map (fun s -> s.P4)
            let (nm, _, _, t4) = classTargets.[c]
            printfn "  %-3s n=%4d  l4 %.9f / %.9f / %.9f  (target %.9f)  max|l2| %.3g  max shell spread %.3g"
                nm arr.Length (Array.min l4s) (Array.average l4s) (Array.max l4s) t4
                (arr |> Array.map (fun s -> abs s.P2) |> Array.max)
                (arr |> Array.map (fun s -> s.ShellSpread) |> Array.max)
spreadReport "train" trainByClass
spreadReport "test" testByClass

// Distortion census: stored samples whose l4 power differs measurably from
// the ideal (cubic site symmetry pins most shells to EXACT rational
// directions — free positional parameters are the only route to distortion).
let distorted =
    Array.append (trainByClass |> Array.collect id) (testByClass |> Array.collect id)
    |> Array.filter (fun s ->
        let (_, _, _, t4) = classTargets.[s.Cls]
        abs (s.P4 - t4) > 1e-9)
printfn "distortion census: %d of %d stored shells measurably off-ideal (|l4 - target| > 1e-9)"
    distorted.Length (nTrain + nTest)
for s in distorted |> Array.truncate 12 do
    let (nm, _, _, t4) = classTargets.[s.Cls]
    printfn "    %-3s COD %d site %d (%s) l4 = %.9f (ideal %.9f)  l3 = %.3g" nm s.CodId s.SiteOrdinal s.SiteLabel s.P4 t4 s.P3

// ---------------------------------------------------------------------------
// Write the store (idempotent: replaced wholesale).
// ---------------------------------------------------------------------------
let root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))
let store = Path.Combine(root, "examples", "data", "cod_shells.zarr")
// Replace the store's CONTENTS rather than the directory itself: a process
// (e.g. a shell) may legitimately have the store as its working directory,
// and Windows refuses to delete a process's cwd. Content-wise idempotence is
// what the hash below certifies.
if Directory.Exists store then
    for f in Directory.GetFiles store do File.Delete f
    for d in Directory.GetDirectories store do Directory.Delete(d, true)

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
    [ fvar "pos_train" [ "samp_tr"; "pt"; "xyz" ] [ nTr; nPad; 3 ] posTr
      fvar "pos_train_yzx" [ "samp_tr"; "pt"; "xyz" ] [ nTr; nPad; 3 ] posTrY
      fvar "pos_test" [ "samp_te"; "pt"; "xyz" ] [ nTe; nPad; 3 ] posTe
      fvar "pos_test_yzx" [ "samp_te"; "pt"; "xyz" ] [ nTe; nPad; 3 ] posTeY
      fvar "y_train" [ "samp_tr"; "klass" ] [ nTr; nClass ] yTr
      fvar "y_test" [ "samp_te"; "klass" ] [ nTe; nClass ] yTe
      ivar "label_train" [ "samp_tr" ] [ nTr ] labTr
      ivar "label_test" [ "samp_te" ] [ nTe ] labTe
      ivar "cn_train" [ "samp_tr" ] [ nTr ] cnTr
      ivar "cn_test" [ "samp_te" ] [ nTe ] cnTe
      fvar "cnf_train" [ "samp_tr" ] [ nTr ] cnfTr
      fvar "cnf_test" [ "samp_te" ] [ nTe ] cnfTe
      ivar "cod_train" [ "samp_tr" ] [ nTr ] codTr
      ivar "cod_test" [ "samp_te" ] [ nTe ] codTe
      ivar "site_train" [ "samp_tr" ] [ nTr ] siteTr
      ivar "site_test" [ "samp_te" ] [ nTe ] siteTe
      fvar "pow_train" [ "samp_tr"; "powk" ] [ nTr; 3 ] powTr
      fvar "pow_test" [ "samp_te"; "powk" ] [ nTe; 3 ] powTe
      fvar "rot3" [ "rotk"; "nine" ] [ nRot; 9 ] rot3 ]

// Deterministic content hash of the whole store: sorted relative paths, then
// path bytes + file bytes through one SHA-256. Two runs must print the same.
let storeHash =
    use sha = SHA256.Create()
    let hfiles =
        Directory.GetFiles(store, "*", SearchOption.AllDirectories)
        |> Array.map (fun f -> f.Substring(store.Length).Replace('\\', '/'))
        |> Array.sort
    use ms = new MemoryStream()
    for f in hfiles do
        let pb = Text.Encoding.UTF8.GetBytes(f)
        ms.Write(pb, 0, pb.Length)
        let fb = File.ReadAllBytes(Path.Combine(store, f.TrimStart('/')))
        ms.Write(fb, 0, fb.Length)
    sha.ComputeHash(ms.ToArray()) |> Array.map (sprintf "%02x") |> String.concat ""

// ---------------------------------------------------------------------------
// Pins.
// ---------------------------------------------------------------------------
let sumsq (a: float[]) = a |> Array.sumBy (fun v -> v * v)
printfn "wrote %s" store
printfn "  store hash = %s" storeHash
printfn "  train %d + test %d samples, padded to %d points, %d classes" nTr nTe nPad nClass
printfn "  example provenance: first train sample of each class:"
for c in 0 .. nClass - 1 do
    if trainByClass.[c].Length > 0 then
        let s = trainByClass.[c].[0]
        let (nm, _, _, _) = classTargets.[c]
        printfn "    %-3s COD %d site %d (%s) sg %d CN %d" nm s.CodId s.SiteOrdinal s.SiteLabel s.SgNum s.Cn
printfn "  pos_train[0,0,:] = %.17g %.17g %.17g" posTr.[0] posTr.[1] posTr.[2]
printfn "  pos_test[0,0,:]  = %.17g %.17g %.17g" posTe.[0] posTe.[1] posTe.[2]
printfn "  sum(pos_train^2) = %.17g   sum(pos_test^2) = %.17g" (sumsq posTr) (sumsq posTe)
printfn "  sum over samples of stored l4 power: train %.17g  test %.17g"
    (Array.init nTr (fun s -> powTr.[s * 3 + 2]) |> Array.sum)
    (Array.init nTe (fun s -> powTe.[s * 3 + 2]) |> Array.sum)
