// Interpreter driver (Milestone M0).
//
// Wraps the tree-walking evaluator (Blade.Interp.Core) and value printer
// (Blade.Interp.Print) into a single process-like entry point, runProgram,
// whose (ExitCode, Stdout, Stderr) result the differential gate
// (tests/InterpDiff.fs) diffs against the compiled C++ binary produced by
// CodeGen. The driver owns only sequencing, output assembly, and the mapping
// of every failure mode to the gate's exit-code protocol; it evaluates and
// prints nothing itself.
//
// Compiled inside Blade.fsproj after Interp/{Value,CppFormat,Numerics,
// RandMirror}.fs and after Core/Print, so it references the concrete IR and
// the sibling interpreter modules directly.
module Blade.Interp.Run

open System.Text
open Blade.Types
open Blade.IR
open Blade.IRLoopStructure
open Blade.IRStorage
open Blade.IRLift
open Blade.IRMono
open Blade.IRPrint
open Blade.IRValidate
open Blade.Interp.Value

/// The process-like result of one interpreter run -- the triple the
/// differential gate compares, mirroring an OS process's exit code + streams.
type InterpResult =
    { ExitCode: int
      Stdout: string
      Stderr: string }

/// What a completed run leaves behind for the NEXT run of a session that
/// EXTENDS it (`ReplSession`, notebook/REPL lanes).
///
/// The REPL re-lowers and re-runs the whole accumulated session on every
/// submission, because SSA IRIds are minted fresh by each lowering pass, so a
/// root Env keyed by IRId cannot survive into the next pass (Interp/Repl.fs).
/// That argument is about the Env, not about the VALUES: a top-level binding
/// whose defining source and preceding session are unchanged computes the same
/// value whatever ids it drew this time. Keying on the binding NAME -- the one
/// handle that IS stable across passes -- lets the next run adopt those values
/// and skip their initializers, which is what turns a session's cost from
/// quadratic in cell count into linear.
///
/// CORRECTNESS RESTS ON THE CALLER'S PREFIX RULE: a memo may only be offered to
/// a run whose session snippets START WITH exactly the snippets that produced
/// it (ReplSession.EvalCandidate). Then these values are the true end state of
/// running that prefix -- including any mutation a later prefix binding
/// performed on an earlier one, since the memo was taken after the prefix had
/// wholly run -- and the names it carries are exactly the prefix's, because
/// top-level names are unique within a session and a redefinition splices in
/// place (so it changes the snippet list and misses the prefix test).
type SessionMemo =
    { /// name -> (the IR type the binding had when cached, its value).
      Values: Map<string, IRType * Value>
      /// Could a snippet write THROUGH an earlier binding, rather than just
      /// rebinding a name? Only a `mut` array parameter can (element writes
      /// alias the caller), so this is set by the caller from the session's
      /// own declarations. It licenses reusing a PROPER prefix of the cached
      /// snippets: without it, a cached snippet the new candidate drops might
      /// have written into a binding being kept, and the kept values would no
      /// longer describe the prefix alone.
      ///
      /// Deliberately NOT derived from IRModule.MutableArrayLets: that set
      /// exists for `let mut a = Z` deep-copy semantics and also catches the
      /// buffer a `let rec` array is built in, which is the compiler's own
      /// scratch space and no hazard at all. Using it made every recursive-
      /// array session opt out of the prefix reuse for no reason.
      MutationFree: bool
      /// Names whose evaluation is what produces this session's display frames
      /// (see the `emitted` bracketing in execProgram). Recorded so the fact
      /// survives into later runs: such a binding may only be adopted when
      /// `Frames` also carries what it emitted, since a binding that does not
      /// re-run cannot re-emit, and a plot that stopped re-emitting would
      /// vanish from the editor's panel.
      FrameEmitters: Set<string>
      /// For each frame emitter, the frames it emitted, ready to REPLAY.
      /// Cached beside the value and sound for the same reason: under the
      /// prefix rule a binding eligible for adoption had the same inputs, so
      /// the frames it would emit now are the frames it emitted then.
      /// Replaying them is what lets an unchanged plot cost nothing on the
      /// next cell's evaluation instead of recomputing its whole field --
      /// which, for a notebook whose cells render, is the difference between
      /// a fixed per-evaluation tax the size of every plot in the session and
      /// no tax at all.
      Frames: Map<string, (string * bool * bool) list> }

let emptyMemo : SessionMemo =
    { Values = Map.empty; MutationFree = false; FrameEmitters = Set.empty; Frames = Map.empty }

/// INSTRUMENT (session-memo tests only; the compiler never reads them). How
/// many top-level bindings the most recent `runProgramMemo` ADOPTED from its
/// incoming memo -- initializer skipped -- and how many it had to evaluate.
/// "The cache survived" has no other observable: a memo hit and a recompute
/// print the same value, and wall clock is not a pin. Overwritten per run;
/// single-threaded by construction, since the REPL/notebook lanes evaluate one
/// candidate at a time in one process.
let mutable lastMemoAdopted = 0
let mutable lastMemoEvaluated = 0

/// Does a cached type still describe the same value SHAPE as the binding's
/// freshly lowered type? Deliberately NOT structural equality, and that is the
/// whole point of this function existing.
///
/// An `IRIndexTypeG.Id` is an SSA ordinal minted by the lowering pass that
/// produced it (`IRBuilder.FreshId`, counting up from 0 in declaration order).
/// Two passes over the SAME session therefore hand one array type DIFFERENT
/// ids as soon as anything ahead of it consumes a different number of ids --
/// and a later cell that triggers fresh domain elaboration does exactly that,
/// because `Blade.ML.Elaborate.expand` splices its generated functions at the
/// FRONT of the module (so every user declaration shifts). `Blade.Grad.expand`
/// splices each synthesized derivative BESIDE ITS ROOT declaration instead,
/// which is why `ad.grad` in a later cell never showed this and `ml.*` always
/// did. Comparing ids rejected every array binding in the session over a
/// difference the memo's own doc calls unpreservable -- it is precisely why
/// the cache is keyed on NAMES and not on ids.
///
/// What the guard is actually FOR is the inference hazard: Blade infers across
/// the whole session, so a later cell can pin an earlier binding's element
/// type or its rank, and a value cached under the old type would be the wrong
/// shape. `canonTypeKey` (IRMono, written for HM specialization dedup) is
/// already the occurrence-id-INDEPENDENT structural key -- same element type,
/// rank, extent and symmetry => same key -- so it decides exactly that
/// question and nothing else. Its opaque catch-all ("T", for the type forms it
/// does not describe) is excluded: such a type can still be adopted, but only
/// on exact equality, never on a key match.
let private memoTypeAgrees (cachedTy: IRType) (nowTy: IRType) : bool =
    cachedTy = nowTy
    || (let k = canonTypeKey cachedTy
        k <> "T" && k = canonTypeKey nowTy)

/// Is this value safe to carry across a lowering boundary?
///
/// Only self-contained MATERIALIZED data is. `VDeferred` and `VLoopObj` close
/// over an `Env` and `VClosure` over capture cells keyed by IRId; adopting one
/// into a freshly lowered program would resurrect ids from a dead pass. They
/// are also precisely the CHEAP values -- a deferred binding has not computed
/// anything yet -- so refusing them costs nothing. The expense this memo exists
/// to remove sits in `|> compute`-materialized arrays, which carry no env.
///
/// VCompound/VSparse are excluded deliberately rather than by oversight: they
/// pair a buffer with an index table, and until that pairing is shown to be
/// env-free the conservative answer is to recompute them.
let rec private memoizableValue (v: Value) : bool =
    match v with
    | VInt _ | VInt32 _ | VFloat _ | VFloat32 _ | VComplex _
    | VBool _ | VString _ | VChar _ | VUnit -> true
    | VArray _ -> true
    | VTuple vs -> Array.forall memoizableValue vs
    | VStruct (_, fields) -> fields |> Array.forall (snd >> memoizableValue)
    | _ -> false

// Exit-code protocol (mirrors the C++ runtime plus a private interpreter lane):
//   0   - normal completion.
//   1   - InterpPanic: a Blade runtime guard fired. Matches blade_rt::panic,
//         which prints the diagnostic and std::exit(1) (cpp/blade_runtime.hpp).
//   125 - a feature the interpreter/printer does not implement yet
//         (Core.InterpUnsupported / Print.PrintUnsupported). A distinct code so
//         the gate classifies SKIP-UNSUPPORTED apart from a real divergence.
//   70  - any other .NET exception escaping the run: an interpreter bug, not a
//         program fault (70 == BSD EX_SOFTWARE, "internal software error").

[<Literal>]
let ExitOk = 0

[<Literal>]
let ExitPanic = 1

[<Literal>]
let ExitUnsupported = 125

[<Literal>]
let ExitInterpBug = 70

/// Format an InterpPanic byte-for-byte like cpp/blade_runtime.hpp:29-41's
/// blade_rt::panic: an `error[CODE]: msg\n` line, then a `  --> file:line\n`
/// location line when a span is carried (file present and line > 0), then the
/// Blade shadow-stack frames innermost-first (`frames` is already in that
/// order -- Core.capturedFrames walks `depth-1 .. 0`). Each frame carries
/// file=nullptr and line=0 (`BLADE_FRAME(name, nullptr, 0)`), so panic's
/// `if (stack[i].file && stack[i].line > 0)` guard is always false: a frame
/// line is exactly `  at <name>\n`, no ` (file:line)` suffix.
let private formatPanic (code: string) (msg: string) (file: string option) (line: int) (frames: string list) : string =
    let sb = StringBuilder()
    sb.Append("error[").Append(code).Append("]: ").Append(msg).Append('\n') |> ignore
    match file with
    | Some f when line > 0 ->
        sb.Append("  --> ").Append(f).Append(':').Append(line).Append('\n') |> ignore
    | _ -> ()
    for fn in frames do
        sb.Append("  at ").Append(fn).Append('\n') |> ignore
    sb.ToString()

/// Assemble the printable module for a (possibly multi-module) program. M0
/// corpus programs are single-module; for a merged multi-module program the
/// printer runs over one synthetic module carrying every binding in module
/// order, exactly as CodeGen.genSelfContainedProgramFromIR merges modules
/// (functions first, bindings concatenated in module order).
let private printableModule (program: IRProgram) : IRModule =
    match program.Modules with
    | [ single ] -> single
    | many ->
        { many.Head with
            Functions = many |> List.collect _.Functions
            Bindings = many |> List.collect _.Bindings
            MutableArrayLets = many |> List.fold (fun acc m -> Set.union acc m.MutableArrayLets) Set.empty }

// Random-fill bindings (rand.<fam>, RandomInits/RandGen).
//
// Lowering records a `let A = rand.<kind>(key, params.., shape)` binding with a
// unit placeholder Value and its RandGen(kind, keyIR, parIRs) in
// IRModule.RandomInits. CodeGen materializes it at the binding's position via
// genRandGenBinding: allocate the dense pool, then ONE
// `blade_rand::<kind>(pool_base(A.data), card, (int64_t)(key)[, (double)(p)..])`
// call (card = product of extents, row-major flat pool, one draw per slot). The
// interpreter mirrors this so output prints byte-for-byte like the compiled
// binary; RandMirror.draws reproduces the mt19937_64 stream bit-exactly.

/// Truncate a key value toward zero to int64, exactly as codegen's cast
/// `(int64_t)(key)` (mirrors Core.toI64, which is private to Core).
let private keyToInt64 (v: Value) : int64 =
    match v with
    | VInt n -> n
    | VInt32 n -> int64 n
    | VFloat f -> int64 f
    | VFloat32 f -> int64 (float f)
    | VBool b -> if b then 1L else 0L
    | VChar c -> int64 (int c)
    | _ -> 0L

/// Widen a distribution-parameter value to float, exactly as codegen's
/// `(double)(p)` cast (the checker already types these against Float64, so the
/// integer arms only cover a literal that promoted).
let private parToFloat (v: Value) : float =
    match v with
    | VFloat f -> f
    | VFloat32 f -> float f
    | VInt n -> float n
    | VInt32 n -> float n
    | VBool b -> if b then 1.0 else 0.0
    | VChar c -> float (int c)
    | _ -> 0.0

/// Unwrap the categorical weights argument to the flat `double*` pool codegen
/// hands the runtime. The checker guarantees a dense rank-1 Float64 array, so
/// its store is a plain SFloat; anything else means the guarantee was lost
/// upstream and is raised rather than coerced. `k` is the checker-pinned extent
/// codegen passes as the length -- a disagreement with the actual pool would
/// desynchronize interpreter and binary, so it is caught here instead.
let private weightsPool (v: Value) (k: int) : float[] =
    match v with
    | VArray ba ->
        match ba.Data with
        | SFloat data when data.Length = k -> data
        | SFloat data ->
            raise (Core.InterpUnsupported
                     $"rand.categorical weights pool has {data.Length} elements but the pinned extent is {k}")
        | _ -> raise (Core.InterpUnsupported "rand.categorical weights are not a dense Float64 pool")
    | _ -> raise (Core.InterpUnsupported "rand.categorical weights did not evaluate to an array")

/// Materialize a `rand.<fam>` binding as CodeGen.genRandGenBinding emits it.
/// Component extents come from the binding's ArrayElem type (one entry per rank
/// component, all static IRLitInt -- codegen `#error`s otherwise). card
/// = product of extents; the key, the family's runtime Float64 parameters and
/// (for categorical) the weights array are evaluated in the ROOT env (they may
/// reference earlier bindings) and cast as codegen casts them; RandMirror draws
/// `card` values keyed by it. The flat pool is reshaped via
/// ArrayOps.mkDenseArray, exactly as every other dense interpreter array is.
///
/// STORE TYPE follows the binding's element type, which is the same fork the
/// C++ pool type takes: categorical fills an SInt store from `int64[]` draws,
/// every other family an SFloat store from `float[]` draws. Routing categorical
/// through the float path would print its indices as `0` vs `0.0`-formatted
/// doubles and break byte-parity with the binary.
let private materializeRandGen (state: Core.InterpState) (root: Env) (binding: IRBinding) (kind: string) (keyExpr: IRExpr) (parExprs: IRExpr list) (weightsExpr: (IRExpr * int) option) : Value =
    match binding.Type with
    | ArrayElem arrTy ->
        let extents =
            arrTy.IndexTypes
            |> List.collect (fun idx ->
                List.replicate idx.Rank
                    (match idx.Extent with
                     | IRLit (IRLitInt n) -> n
                     | _ -> raise (Core.InterpUnsupported "rand binding with a non-literal extent")))
        let card = extents |> List.fold (*) 1L
        let key = keyToInt64 (Core.evalExpr state root keyExpr)
        // Params are evaluated ONCE, before the fill, left-to-right -- matching
        // the single C++ call whose arguments are evaluated before the loop.
        let pars = parExprs |> List.map (fun p -> parToFloat (Core.evalExpr state root p))
        // .NET arrays are int-indexed, so the draw count is int-bounded exactly
        // as the pool it fills; card stays int64 to match codegen's `1L` fold.
        let store =
            match weightsExpr with
            | Some (wExpr, k) ->
                let w = weightsPool (Core.evalExpr state root wExpr) k
                SInt (RandMirror.drawsCategorical kind key w (int card))
            | None -> SFloat (RandMirror.draws kind key pars (int card))
        state.Cells <- state.Cells + card
        VArray (ArrayOps.mkDenseArray arrTy.ElemType arrTy.IndexTypes (Array.ofList extents) store)
    | _ -> raise (Core.InterpUnsupported "rand binding is not an array type")

// Provider reads (`let A = view |> alias.read` over a netcdf/zarr var).
//
// Lowering records a deferred provider read in IRModule.ProviderReads (keyed by
// the receiving binding's IRId; IR.fs ProviderReadSpec) and CodeGen materializes
// it at the binding's position via genProviderReadBinding (CodeGen.fs ~L8296):
// dispatched on the registered ProviderSpec (Blade.ProviderRegistry), it emits
// the provider's runtime C++ reader (nc_get_var_* / zarr fstream chunk reads).
//
// The interpreter mirrors that in-process: at the binding's position (exactly
// like RandomInits), it invokes the registered F# provider's compile-time
// whole-payload reader -- ProviderSpec.ReadVarData, the same entry point the
// static fold (ProviderStatics.readAndFold) uses -- and shapes the result into
// a dense BladeArray of the variable's declared type.
//
// CWD ASYMMETRY (load-bearing gotcha). spec.FilePath is the store path baked
// as given in the source. The compiled binary resolves it against its own cwd
// (the exe's directory at runtime); the interpreter resolves it against the
// compiler process cwd. A relative path reads identical bytes on both sides
// only when the fixture is staged at both locations (the two-copy scheme
// NetcdfTests/ZarrTests use). An absolute path is cwd-independent and always
// agrees. ReadVarData is called with the path verbatim (no rewriting).
//
// SCOPE: only dense whole-variable reads are mirrored. The packed
// (SymIdx/AntisymIdx) arm needs compact-pool storage and ReadVarData refuses
// packed vars; the compound (load_compound mask) arm produces a
// Compound<T,rank>; windowed reads are packed sub-simplices; a streamed read
// is never materialized (consuming nests inline fibers). Each raises
// InterpUnsupported so the whole program SKIP-classifies rather than risk
// wrong bytes. Provider writes (alias.write) are a side effect, gated by the caller.

/// Materialize a deferred provider read as CodeGen.genProviderReadBinding's DENSE
/// arm does, but in-process (see the section header for the cwd asymmetry and
/// gated non-dense arms). Extents come from the payload's own DimLengths; the
/// element/index types come from the spec's VarType. Narrow element types
/// widen into the wide store (Float32 -> SFloat, Int32 -> SInt); Print
/// narrows back at format time, matching `cout << (float)` / `(int32_t)`.
let private materializeProviderRead (state: Core.InterpState) (binding: IRBinding) (spec: ProviderReadSpec) : Value =
    if spec.Streamed then
        raise (Core.InterpUnsupported "streamed provider read (.stream -- per-fiber reads not interpreted)")
    elif spec.MaskName.IsSome then
        raise (Core.InterpUnsupported "compound (load_compound) provider read (M2.7 compound family)")
    elif spec.Window.IsSome then
        raise (Core.InterpUnsupported "windowed packed provider read (read_window)")
    // Ahead of the packed gate (which a wreath group also trips) so the wreath
    // arm owns it: a wreath array is a flat pool, not an Array-with-a-skeleton,
    // and `mkDenseArray` below would shape it as a dense prod(ri)-axis tensor.
    // Materialized here from the provider's canonical-pool reader, cell for
    // cell. A provider with no wreath pools (ReadWreathPool = None) still
    // refuses, and it FAILS rather than SKIPs: InterpUnsupported would
    // classify as SKIP-UNSUPPORTED and let a divergence hide behind it.
    elif spec.VarType.IndexTypes |> List.exists (fun ix -> ix.Symmetry = SymWreath) then
        let ix = spec.VarType.IndexTypes |> List.find (fun ix -> ix.Symmetry = SymWreath)
        let levels = Blade.IR.orbitLevelsOf ix
        let refuse (why: string) =
            failwith (Blade.IR.orbitStorageUnsupported
                          $"provider read of '{spec.VarName}' ({why})" levels)
        match Blade.ProviderRegistry.tryFind spec.Provider with
        | None -> refuse $"provider '{spec.Provider}' is not registered"
        | Some pspec ->
        match pspec.ReadWreathPool with
        | None -> refuse $"provider '{spec.Provider}' stores no OrbIdx pools"
        | Some readPool ->
            if spec.VarType.IndexTypes.Length <> 1 then
                refuse "a wreath group combined with other index groups has no pool layout"
            let n =
                match Blade.IR.orbitBaseExtent ix with
                | IRLit (IRLitInt v) -> v
                | _ -> refuse "a wreath class needs a compile-time extent"
            match readPool spec.FilePath spec.VarName with
            | Error e ->
                // Unlike the dense arm below, this is NOT a SKIP: the compiled
                // side reads the same store from the same path, so a failure
                // here is a real disagreement, not an un-interpreted feature.
                failwith $"provider read of '{spec.VarName}' from '{spec.FilePath}': {e}"
            | Ok data ->
                // The pool arrives as ONE axis of `cardinality` cells; allocWreath
                // sizes the store from cellCountChecked (the same fold that
                // validated shape[0] at metadata parse), so a length disagreement
                // here is the store lying about its own class.
                let arr = ArrayOps.allocWreath spec.VarType.ElemType spec.VarType.IndexTypes levels n
                let cells = ArrayOps.wreathCellCount arr
                let got = data.DimLengths |> List.fold (*) 1
                if got <> cells then
                    failwith $"provider read of '{spec.VarName}': the store's pool holds {got} cells but OrbIdx{(Blade.IR.ppOrbitLevels levels)} at extent {n} has {cells}"
                let put (i: int) (v: Value) = ArrayOps.wreathWriteAt arr (int64 i) v
                (match ArrayOps.elemThrough spec.VarType.ElemType, data.Payload with
                 | Some (ETFloat64 | ETFloat32), Blade.ProviderRegistry.PFloats xs -> xs |> Array.iteri (fun i x -> put i (VFloat x))
                 | Some (ETFloat64 | ETFloat32), Blade.ProviderRegistry.PInts xs -> xs |> Array.iteri (fun i x -> put i (VFloat (float x)))
                 | Some (ETInt64 | ETInt32), Blade.ProviderRegistry.PInts xs -> xs |> Array.iteri (fun i x -> put i (VInt x))
                 | Some (ETInt64 | ETInt32), Blade.ProviderRegistry.PFloats xs -> xs |> Array.iteri (fun i x -> put i (VInt (int64 x)))
                 | _ ->
                     raise (Core.InterpUnsupported
                             $"provider read of '{spec.VarName}' into a non-numeric element type"))
                state.Cells <- state.Cells + int64 cells
                VArray arr
    elif spec.VarType.IndexTypes |> List.exists (fun ix -> ix.Symmetry <> SymNone && ix.Rank >= 2) then
        raise (Core.InterpUnsupported "packed (symmetric/antisymmetric) provider read")
    else
        match Blade.ProviderRegistry.tryFind spec.Provider with
        | None ->
            raise (Core.InterpUnsupported $"provider '{spec.Provider}' is not registered (ProviderStatics.install)")
        | Some pspec ->
            match pspec.ReadVarData spec.FilePath spec.VarName with
            | Error e ->
                // The provider could not read this variable in-process (missing
                // store relative to the compiler cwd, a packed var ReadVarData
                // refuses, a corrupt chunk, ...). No faithful image -> SKIP rather
                // than diverge; the caller classifies SKIP-UNSUPPORTED.
                raise (Core.InterpUnsupported $"provider read of '{spec.VarName}' from '{spec.FilePath}': {e}")
            | Ok data ->
                let arrTy = spec.VarType
                // BL8012 twin of CppNetcdf.ncShapeGuard: the file's rank and
                // dimension lengths at RUN time must match the extents lowering
                // baked from the compile-time file. Literal extents only -- a
                // non-literal extent has nothing to compare, and the compiled
                // side emits no check for it either. Before this guard the
                // array below took its extents from the file while its TYPE
                // kept the baked ones, so a changed file read silently.
                let baked =
                    arrTy.IndexTypes |> List.map (fun ix ->
                        match ix.Extent with
                        | IRLit (IRLitInt n) -> Some n
                        | _ -> None)
                let observed = data.DimLengths |> List.map int64
                if baked.Length <> observed.Length then
                    raise (InterpPanic ("BL8012",
                                        $"NetCDF variable '{spec.VarName}' in '{spec.FilePath}' has rank {observed.Length} at run time; the program was compiled against rank {baked.Length}",
                                        None, 0))
                List.zip baked observed
                |> List.iteri (fun d (b, o) ->
                    match b with
                    | Some n when n <> o ->
                        raise (InterpPanic ("BL8012",
                                            $"NetCDF variable '{spec.VarName}' in '{spec.FilePath}' has dimension {d} of length {o} at run time; the program was compiled against {n}",
                                            None, 0))
                    | _ -> ())
                let extents = observed |> Array.ofList
                let store =
                    match ArrayOps.elemThrough arrTy.ElemType, data.Payload with
                    | Some (ETFloat64 | ETFloat32), Blade.ProviderRegistry.PFloats xs -> SFloat xs
                    | Some (ETFloat64 | ETFloat32), Blade.ProviderRegistry.PInts xs -> SFloat (xs |> Array.map float)
                    | Some (ETInt64 | ETInt32), Blade.ProviderRegistry.PInts xs -> SInt xs
                    | Some (ETInt64 | ETInt32), Blade.ProviderRegistry.PFloats xs -> SInt (xs |> Array.map int64)
                    | _ ->
                        raise (Core.InterpUnsupported
                                $"provider read of '{spec.VarName}' into a non-numeric element type")
                let card = extents |> Array.fold (fun acc e -> acc * e) 1L
                state.Cells <- state.Cells + card
                VArray (ArrayOps.mkDenseArray arrTy.ElemType arrTy.IndexTypes extents store)

/// Execute a program: build state, evaluate every top-level binding in module
/// order into the root env (keyed by the binding's globally-unique IRId -- the
/// SSA scoping discipline in Interp/Value.fs), then print. Raising evaluators
/// propagate out to runProgram's handler.
let private execProgram (state: Core.InterpState) (merged: IRModule) (program: IRProgram)
                        (testName: string) (memoIn: SessionMemo)
                        (printOnly: Set<string> option) : InterpResult * SessionMemo =
    // Display frames are per-RUN state (their `meta.id` ordinals restart), so
    // a re-run of the same session produces the same ids and the editor's plot
    // panel updates its entries in place instead of appending duplicates.
    // Frames are NEVER replayed from a memo: the bindings that emit them are
    // excluded from it below, so they re-run and re-emit every pass, and the
    // ids stay the dense 0..n-1 sequence the editor expects.
    Blade.Display.Frame.resetRun ()
    let root = envNew ()
    // Bindings whose evaluation emitted a display frame this run. They are
    // barred from the outgoing memo so that they re-run next time; a restored
    // binding produces no frame, and a plot that stopped re-emitting would
    // vanish from the panel.
    let frameEmitters = System.Collections.Generic.HashSet<string>(memoIn.FrameEmitters)
    // What each emitter emitted THIS run -- whether it recomputed them or
    // replayed them from the incoming memo. Published so the next run can
    // replay in turn, which is what keeps a warm session warm.
    let outFrames = System.Collections.Generic.Dictionary<string, (string * bool * bool) list>()
    // The run's own memo tally. Zeroed HERE rather than published at the end,
    // so a run that raises partway leaves a truthful partial count instead of
    // the previous run's total.
    lastMemoAdopted <- 0
    lastMemoEvaluated <- 0
    // Function bodies may reference module-level bindings (emitted as
    // main-local capturing lambdas in C++) -- expose the root scope to call
    // frames before any binding evaluates.
    state.Global <- Some root
    for m in program.Modules do
        for b in m.Bindings do
          // A memo hit adopts the cached value and SKIPS the initializer --
          // the whole point of the cache. The type guard is load-bearing:
          // Blade infers across the whole session, so a later cell can pin an
          // earlier binding's element type (or its rank), and a value cached
          // under the old type would then be the wrong shape. Shapes differ ->
          // fall through and recompute (`memoTypeAgrees` says what "differ"
          // means, and why it is not `=`).
          let framesBefore = Blade.Display.Frame.emitted ()
          let producedBefore = Blade.Display.Frame.producedCount ()
          match Map.tryFind b.Name memoIn.Values with
          | Some (cachedTy, cachedV) when memoTypeAgrees cachedTy b.Type
                                          && (not (Set.contains b.Name memoIn.FrameEmitters)
                                              || Map.containsKey b.Name memoIn.Frames) ->
              // An emitter is adopted only WITH its frames, which are replayed
              // here, in the binding's own position -- so the run's frame
              // sequence, ids included, is the one it would have computed.
              match Map.tryFind b.Name memoIn.Frames with
              | Some fs when not (List.isEmpty fs) ->
                  Blade.Display.Frame.replay fs
                  frameEmitters.Add b.Name |> ignore
                  outFrames.[b.Name] <- fs
              | _ -> ()
              lastMemoAdopted <- lastMemoAdopted + 1
              envBind root b.Id cachedV |> ignore
          | _ ->
            lastMemoEvaluated <- lastMemoEvaluated + 1
            // Defer-aware: a deferred combinator binding stores VDeferred (no
            // eager force); a method_for/object_for binding stores VLoopObj;
            // everything else evaluates eagerly, mirroring CodeGen.genBinding.
            // RandomInits / ProviderReads / ProviderWrites bindings are
            // placeholders intercepted here at their position in the binding
            // sequence (so a key expr referencing an earlier binding resolves
            // against the root env, as the C++ counterpart reads earlier
            // main()-locals); the intercept order mirrors CodeGen.genBinding's
            // dispatch (ProviderReads, ProviderWrites, RandomInits, CompoundInits).
            let v =
                match Map.tryFind b.Id m.ProviderReads with
                | Some spec ->
                    materializeProviderRead state b spec
                | None ->
                match Map.tryFind b.Id m.ProviderWrites with
                | Some _ ->
                    // alias.write("path", A): a filesystem side effect. The
                    // interpreter never writes (side-effect policy -- flag-gated
                    // later), so the whole program SKIP-classifies.
                    raise (Core.InterpUnsupported "provider write (alias.write -- side effect; flag-gated later)")
                | None ->
                match Map.tryFind b.Id m.RandomInits with
                | Some (RandGen (kind, keyExpr, parExprs, weightsExpr)) ->
                    materializeRandGen state root b kind keyExpr parExprs weightsExpr
                | Some (FillModulus _) ->
                    // fill_random(mod) fills with C `rand() % mod`: nondeterministic
                    // and NOT mirrored by RandMirror (only the deterministic
                    // mt19937_64 uniform/normal streams are), so no byte-parity is
                    // possible -- classify SKIP-UNSUPPORTED.
                    raise (Core.InterpUnsupported "fill_random(mod) (C rand()%mod is nondeterministic)")
                | None ->
                    // A CompoundInits binding (compound(A, m) / load_compound)
                    // materializes its compact buffer + rank-to-tuple table here, at
                    // its position in sequence, exactly as genCompoundInitBinding
                    // scatters present cells at the binding's site in main().
                    match Map.tryFind b.Id m.CompoundInits with
                    | Some (denseExpr, maskExpr) ->
                        Loops.materializeCompoundBinding state root b denseExpr maskExpr
                    | None ->
                        // A SparseInits binding (sparse(values, keys)) bundles
                        // the rank-1 values buffer with the key table here, at
                        // its position, mirroring genSparseInitBinding.
                        match Map.tryFind b.Id m.SparseInits with
                        | Some valuesExpr ->
                            Loops.materializeSparseBinding state root b valuesExpr
                        | None ->
                            Core.evalBinding state root b
            envBind root b.Id v |> ignore
            if Blade.Display.Frame.emitted () > framesBefore then
                frameEmitters.Add b.Name |> ignore
                outFrames.[b.Name] <- Blade.Display.Frame.producedSince producedBefore

    // Resolve a binding id to its computed value for the printer. Print decides
    // which bindings render and in what order/format (iostream parity), and
    // emits the leading "<name> completed in 0s" timing line (the gate strips
    // timing lines on both sides).
    let lookup (id: IRId) : Value option =
        match envTryFind root id with
        | Some cell -> Some cell.V
        | None -> None

    let sb = StringBuilder()
    // Frames first, one whole line each at column 0 -- the position the
    // compiled binary puts them in (main()'s body precedes the timing line and
    // the print block), which is what keeps the two lanes byte-identical.
    for frame in Blade.Display.Frame.drain () do
        sb.Append(frame).Append('\n') |> ignore
    Print.printBindingsOnly testName lookup state.ForcedDeferred merged printOnly sb

    // The memo this run hands to its successor: every top-level binding that
    // holds carryable data, under the type it holds it at. Bindings that were
    // themselves restored are re-published unchanged -- a session that keeps
    // extending keeps its whole prefix warm.
    let outValues =
        program.Modules
        |> List.collect _.Bindings
        |> List.fold (fun acc b ->
            match envTryFind root b.Id with
            | Some cell when memoizableValue cell.V
                             && (not (frameEmitters.Contains b.Name)
                                 || outFrames.ContainsKey b.Name) ->
                Map.add b.Name (b.Type, cell.V) acc
            | _ -> acc) Map.empty

    // state.Err collects any non-fatal interpreter diagnostics -> stderr.
    ({ ExitCode = ExitOk; Stdout = sb.ToString(); Stderr = state.Err.ToString() },
     { Values = outValues
       // Filled in by the caller, which can see the session's declarations.
       MutationFree = memoIn.MutationFree
       FrameEmitters = Set.ofSeq frameEmitters
       Frames = outFrames |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq })

/// Run a lowered program under the tree-walking interpreter, mapping each
/// outcome onto the exit-code protocol above. The whole run executes on the
/// large stack (Runtime.fs) -- the same worker the compile pipeline uses --
/// because deep recursion arrives in later milestones; catching on that worker
/// thread means no exception ever crosses back to the caller.
///
/// `memoIn` carries values from a run of a session this one EXTENDS (see
/// SessionMemo for the prefix rule the caller must honour); pass `emptyMemo`
/// for a one-shot run. The returned memo is meaningful only on ExitOk -- any
/// other outcome hands back `emptyMemo`, so a failed or partially-executed run
/// can never seed the next one.
let runProgramMemo (program: IRProgram) (testName: string) (limits: InterpLimits)
                   (memoIn: SessionMemo) (printOnly: Set<string> option) : InterpResult * SessionMemo =
    Blade.Runtime.runOnLargeStack (fun () ->
        // Build the interpreter state OUTSIDE execProgram but capture it in a ref
        // the panic handler can read: on an escaping InterpPanic we render the
        // shadow-stack frames still live in the state (evalCall never pops on
        // the exception path). (`ref None` guards makeState itself throwing.)
        let stateRef : Core.InterpState option ref = ref None
        try
            // One merged module drives the callables table AND printing, exactly
            // as CodeGen.genSelfContainedProgramFromIR merges modules for main().
            let merged = printableModule program
            // Install the module's callables into the AsyncLocal AnalysisContext
            // on THIS worker thread so buildLoopNestCodeGen can resolve kernels
            // (via resolveKernel/resolveCallable in Interp/Loops.fs). AsyncLocal
            // does not flow from makeState's private table, so it must be set
            // here and restored on exit. Harmless for a pure-scalar run.
            let savedCtx = Blade.IR.setCallablesContext (Blade.IRPrint.buildCallablesTableForModule merged)
            try
                let state = Core.makeState merged limits
                // Wire the M2 loop/array backend (Interp/Loops.fs).
                let hooks : Core.InterpHooks =
                    { EvalArrayNode = Loops.evalArrayNode
                      Force = Loops.force }
                state.Hooks <- Some hooks
                stateRef.Value <- Some state
                execProgram state merged program testName memoIn printOnly
            finally
                Blade.IR.restoreAnalysisContext savedCtx
        with
        | InterpPanic (code, msg, file, line) ->
            let frames = match stateRef.Value with Some st -> Core.capturedFrames st | None -> []
            ({ ExitCode = ExitPanic; Stdout = ""; Stderr = formatPanic code msg file line frames }, emptyMemo)
        | Core.InterpUnsupported feature ->
            ({ ExitCode = ExitUnsupported; Stdout = ""; Stderr = $"interp-unsupported: {feature}" }, emptyMemo)
        // Array layer's own "not yet interpreted" signal: ArrayOps compiles
        // before Core, so it raises its own ArrayOpUnsupported instead, which
        // must SKIP-classify identically (Interp/ArrayOps.fs CONTRACT NOTE (2)).
        | ArrayOps.ArrayOpUnsupported feature ->
            ({ ExitCode = ExitUnsupported; Stdout = ""; Stderr = $"interp-unsupported: {feature}" }, emptyMemo)
        | Print.PrintUnsupported feature ->
            ({ ExitCode = ExitUnsupported; Stdout = ""; Stderr = $"interp-unsupported: {feature}" }, emptyMemo)
        // Scalar-numerics layer's own signal, on the same footing and for the
        // same reason (Numerics.fs compiles before Core, so it cannot raise
        // InterpUnsupported): a complex intrinsic this build cannot reproduce
        // bit-exactly is a gap in the INTERPRETER, not a fault in the program.
        | Numerics.NumericsUnsupported feature ->
            ({ ExitCode = ExitUnsupported; Stdout = ""; Stderr = $"interp-unsupported: {feature}" }, emptyMemo)
        | ex ->
            ({ ExitCode = ExitInterpBug; Stdout = ""; Stderr = $"interp-error: {ex.Message}" }, emptyMemo))

/// The one-shot form: no memo in, memo discarded. Every caller outside the
/// REPL/notebook session lanes (tests, the differential gate, `blade test
/// interp`) runs a whole program exactly once and wants this.
let runProgram (program: IRProgram) (testName: string) (limits: InterpLimits) : InterpResult =
    fst (runProgramMemo program testName limits emptyMemo None)
