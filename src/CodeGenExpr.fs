// The core expression renderer: one rec-chain from exprToCppCore through
// the materialize*Form family (mask/set-ops/sort/transpose/stack/join/
// decompact/negate/copy/gram/matmul/eigh/solve). Atomic by mutual
// recursion -- do not split further.
module Blade.CodeGenExpr

open Blade.IR
open Blade.IRLoopStructure
open Blade.IRStorage
open Blade.IRLift
open Blade.IRMono
open Blade.IRPrint
open Blade.IRValidate
open Blade.Types
open Blade.EmitCpp
open Blade.ReynoldsCore
open Blade.CodeGenState
open Blade.CodeGenExprSupport

/// Convert IRExpr to C++ expression string
let rec exprToCppCore (subst: SubstMap) (names: Map<IRId, string>) (expr: IRExpr) : string =
    match expr with
    | IRLit lit -> litToCpp lit
    | IRVar (id, _) ->
        // A reference to a lifted callable WITH captures cannot stand bare
        // in value position: the lifted C++ function carries trailing
        // capture params, so the bare name neither converts to a
        // std::function of the surface arity nor direct-calls correctly.
        // Close the captures with an inline forwarding closure -- the same
        // [&]-by-name chain as genCallableWrapper/genVarAliasBinding, valid
        // in any scope where the captured locals are visible (i.e. wherever
        // the lambda literal / partial application appeared). Capture-free
        // callables keep the bare name (a plain function converts fine).
        // The `callable.Id = id` guard matters: resolveCallable also sees
        // THROUGH let-aliases, but an alias var is already a std::function
        // of the surface arity with captures closed (genVarAliasBinding) --
        // render it by name; only the direct lifted-callable reference
        // needs the closure.
        match resolveCallable expr with
        | Some callable when callable.Id = id && not (List.isEmpty callable.Captures) ->
            let safeName = sanitizeCppName callable.Name
            let paramSig =
                callable.Params
                |> List.map (fun p ->
                    match p.Type with
                    | ArrayElem arr -> $"{(cppArrayTypeStr arr)} {p.Name}"
                    | _ -> $"{(irTypeToCpp p.Type)} {p.Name}")
                |> String.concat ", "
            let allArgs =
                (callable.Params |> List.map (_.Name))
                @ (captureForwardArgs names callable.Captures)
                |> String.concat ", "
            $$"""[&]({{paramSig}}) { return {{safeName}}({{allArgs}}); }"""
        | _ ->
            match Map.tryFind id names with
            | Some name -> name
            | None -> $"__v{id}"
    | IRParam (name, _, _) -> name
    | IRHaloUnhash (w, off) ->
        // halo window read over a masked domain: coordinate of the present
        // cell at (center + off). The window param is a pointer into the
        // compound index's contiguous rank_to_tuple table at the center
        // (genElementBindingNew's compound-halo arm), so a signed subscript
        // IS the ordinal step -- self-contained in lifted kernel functions.
        // No int64 cast: the coordinate is size_t -- the Array wrapper's exact
        // operator[] type; a cast would make the wrapper-vs-raw-pointer
        // subscript overloads ambiguous.
        let wS = exprToCppCore subst names w
        $"{wS}[({off}L)][0]"
    | IRBinOp (_, op, l, r) ->
        let lStr = exprToCppCore subst names l
        let rStr = exprToCppCore subst names r
        match op with
        | IRCaret -> $"pow({lStr}, {rStr})"
        | IRMath2 name -> renderMath2 name lStr rStr
        | _ -> emitBinOpWithComplexCoercion op l r lStr rStr inferExprType binOpToCpp
    | IRUnaryOp (IRConj, e) ->
        // conj is std::conj/thrust::conj on complex operands; the identity on
        // reals (mathematically conj(x)=x for real x, and std::conj(double)
        // would wrongly promote to std::complex<double>, so emit the operand
        // bare).
        let inner = exprToCppCore subst names e
        if isComplexType (inferExprType e) then $"""{(complexFnName "conj")}({inner})"""
        else inner
    | IRUnaryOp (op, e) ->
        renderUnaryOpTyped op (inferExprType e) (exprToCppCore subst names e)
    | IRIf (cond, thenBr, elseBr) ->
        $"({(exprToCppCore subst names cond)} ? {(exprToCppCore subst names thenBr)} : {(exprToCppCore subst names elseBr)})"
    | IRTuple exprs ->
        $"""std::make_tuple({(exprs |> List.map (exprToCppCore subst names) |> String.concat ", ")})"""
    | IRFma (a, b, c) ->
        // std::fma is correctly rounded on every libm Blade links (and a
        // single vfmadd on any -march with FMA), so it is bit-identical to
        // the interpreter's Math.FusedMultiplyAdd under any -ffp-contract --
        // contraction only ever fuses SEPARATE nodes, never unfuses this one.
        $"std::fma({(exprToCppCore subst names a)}, {(exprToCppCore subst names b)}, {(exprToCppCore subst names c)})"
    | IRComplex (re, im) ->
        // Determine width from the component type. checkExpr enforces
        // that Complex128 components are Float64 and Complex64 are
        // Float32, so we inspect either component (they match) and pick
        // the corresponding C++ template instantiation.
        let cppType =
            match inferExprType re with
            | IRTScalar ETFloat32 -> complexCppTypeName ETComplex64
            | _ -> complexCppTypeName ETComplex128  // Float64 default
        $"{cppType}({(exprToCppCore subst names re)}, {(exprToCppCore subst names im)})"
    | IRTupleProj (e, i, isFlat) ->
        if not isFlat then
            $"std::get<{i}>({(exprToCppCore subst names e)})"
        else
            // Flat projection into potentially nested tuple -- compute navigation path
            let parentTy = inferExprType e
            let rec findPath (ty: IRType) (targetFlat: int) : int list =
                match ty with
                | IRTTuple ts ->
                    let mutable offset = 0
                    let mutable found = None
                    for idx in 0 .. ts.Length - 1 do
                        if found.IsNone then
                            let count = IR.flattenTupleLeaves ts.[idx] |> List.length
                            if targetFlat < offset + count then
                                match ts.[idx] with
                                | IRTTuple _ -> found <- Some (idx :: findPath ts.[idx] (targetFlat - offset))
                                | _ -> found <- Some [idx]
                            offset <- offset + count
                    found |> Option.defaultValue [i]
                | _ -> [i]
            let path = findPath parentTy i
            path |> List.fold (fun acc idx -> $"std::get<{idx}>({acc})") (exprToCppCore subst names e)
    | IRFieldAccess (obj, field) ->
        $"{(exprToCppCore subst names obj)}.{field}"
    | IRStructLit (typeName, fields) ->
        let fieldInits = fields |> List.map (fun (fname, e) -> 
            $".{fname} = {(exprToCppCore subst names e)}") |> String.concat ", "
        $$"""{{typeName}} { {{fieldInits}} }"""
    | IRIndex (arr, indices, _) ->
        // Carousel substitution (reference equality, see SubstMap): a dense
        // halo window read hoisted to a rotating local renders as that local.
        (match trySubst subst expr with
         | Some local -> local
         | None -> renderIndexExpr subst names arr indices)
    | IRApp (func, args, _) ->
        // Function signatures take Array<T,N> / Ragged<T> wrappers
        // natively, one argument per Blade param. Array args pass through
        // as-is (the wrapper carries its own shape via .extents/.lens/
        // .offsets); no companion-arg synthesis. Non-array args render
        // through exprToCpp normally.
        // A callee that IS a lifted callable with captures (direct
        // reference, not a let-alias: resolveCallable sees through aliases,
        // but an alias var is already a std::function with captures closed
        // -- call that by name) is called directly with the capture args
        // appended, since the lifted signature is regular params + capture
        // params.
        let funcStr, captureArgs =
            match func, resolveCallable func with
            | IRVar (fid, _), Some callable when callable.Id = fid && not (List.isEmpty callable.Captures) ->
                (sanitizeCppName callable.Name,
                 captureForwardArgs names callable.Captures)
            | _ -> (exprToCppCore subst names func, [])
        let argStrs =
            args |> List.collect (fun a ->
                let argStr = exprToCppCore subst names a
                match a, inferExprType a with
                | (IRVar _ | IRParam _), ArrayElem _ -> [argStr]
                | _ -> [argStr])
        $"""{funcStr}({(argStrs @ captureArgs |> String.concat ", ")})"""
    | IRLet (id, value, body) ->
        renderLetExpr subst names id value body
    | IRMethodFor _ -> exprError "loop object used as value"
    | IRObjectFor _ -> exprError "loop object used as value"
    | IRApplyCombinator _ | IRComposeApply _ ->
        exprError "unevaluated computation used as value - use |> compute"
    | IRReduceCompute _ ->
        // Statement-shaped (declares accumulators + a loop nest); no IIFE
        // form yet. Reached only if a fused reduce lands in expression
        // position -- bind it to a `let` first.
        exprError "reduce over a deferred computation must be bound to a let (expression position is not supported yet)"
    | IRCompute inner -> 
        // compute forces evaluation of a lazy computation
        match inner with
        | IRApplyCombinator info -> genApplyCombinatorExpr subst names info
        | _ -> exprToCppCore subst names inner  // For non-combinator compute, just evaluate
    | IRPure e -> exprToCppCore subst names e     // pure wraps value
    | IRRank arr -> 
        // Rank is known statically from the type
        let rank = match inferExprType arr with
                   | ArrayElem at -> arrayRank at
                   | _ -> 0
        $"{rank}L"
    | IRExtent (arr, dim) ->
        renderExtentExpr subst names arr dim
    | IRReduce (arrExpr, kernelExpr, initExpr) ->
        renderReduceExpr subst names arrExpr kernelExpr initExpr
    | IRProdSum args ->
        // Fused product-sum sum_t prod_L args[L][t]: one loop, one accumulator,
        // rendered as an IIFE so it composes in any expression position --
        // most importantly inside method_for kernels, where the moment
        // formers' fiber kernels land. Bound comes from the first operand
        // (TypeCheck rejects provably mismatched static extents), through the
        // shared literal-or-runtime rule so a monomorphized `Idx<n>` fiber
        // gives GCC its trip count instead of an opaque pointer load.
        let argStrs = args |> List.map (exprToCppCore subst names)
        let headTy = inferExprType (List.head args)
        let elemStr =
            match headTy with
            | ArrayElem at -> elemTypeToCpp at.ElemType
            | t -> elemTypeToCpp t
        let product = argStrs |> List.map (fun a -> $"{a}[__pt]") |> String.concat " * "
        // A peeled ragged/grouped ROW param is a RaggedRow<T>, which carries
        // its length inline as `.len` and has no `.extents` at all -- the same
        // rule mask / reduce / extents already apply. Reached by the grouped
        // co-iteration `method_for(zip(ga, gb)) <@> lambda(ra, rb) ->
        // prodsum(ra, rb)`, where every operand is such a row.
        let prodSumBound (ty: IRType) (nameStr: string) =
            match ty with
            | ArrayElem a when isRaggedRowType a -> $"{nameStr}.len"
            | _ -> literalOrRuntimeExtent ty nameStr 0
        let serialBound = prodSumBound headTy (List.head argStrs)
        let serialForm =
            $"[&]() {{ {elemStr} __ps = 0; for (size_t __pt = 0; __pt < {serialBound}; __pt++) {{ __ps += {product}; }} return __ps; }}()"
        if not (fpReassocEnabled ()) then serialForm
        else
            // BLADE_FP_REASSOC: the same summation, reassociated. `prodsum`'s
            // summation IS a builtin `+`, so the licence class is the one
            // `foldKernelBuiltinOp` grants outright -- what the knob supplies
            // is the opt-in, which an intrinsic has no where-clause to carry.
            //
            // NO WORKSHARING OpenMP either way: this IIFE routinely sits inside
            // an already-parallel nest (the comm-triangular covariance loop),
            // where a `parallel for` would nest teams. `omp simd` is a SIMD
            // construct, creates no team, and is legal exactly there.
            //
            // Operands are re-spelled ~3K times by the lane form. A plain
            // identifier (the peeled row / array name that reaches here in
            // practice) is repeated verbatim, which keeps any BLADE_RESTRICT
            // qualification on the pointer it names; anything else is bound
            // once to an `auto&&` alias so a compound operand expression is
            // evaluated exactly once, as it is today.
            let isPlainName (s: string) =
                s.Length > 0
                && (System.Char.IsLetter s.[0] || s.[0] = '_')
                && s |> Seq.forall (fun c -> System.Char.IsLetterOrDigit c || c = '_')
            let needAlias = argStrs |> List.exists (isPlainName >> not)
            let opNames =
                if needAlias then argStrs |> List.mapi (fun i _ -> $"__pa{i}")
                else argStrs
            let aliasDecls =
                if not needAlias then []
                else List.map2 (fun n s -> $"auto&& {n} = {s};") opNames argStrs
            let prodAt (i: string) =
                opNames |> List.map (fun a -> $"{a}[{i}]") |> String.concat " * "
            let boundOn = prodSumBound headTy (List.head opNames)
            // WHICH FORM. `omp simd reduction(+:__ps)` where the element type
            // admits it (`simdReducibleElem`), the K-lane chains otherwise.
            //
            // MEASURED over every shape that reaches this site (ST,
            // OMP_NUM_THREADS=1, medians over 3 processes x 5 reps, round-robin;
            // `bench\blade` and `bench\sym` full config, this machine):
            //
            //   gemv row fiber, 3001 x 2999, 2 streams
            //       serial 7.16 ms   8 lanes 7.44 ms (0.96x)   simd 2.76 ms (2.60x)
            //   comoment2, 201 x 10007, 2 streams
            //       serial  163 ms   8 lanes 43.6 ms (3.74x)   simd 53.2 ms (3.07x)
            //   comoment3, 61 x 2003, 3 streams
            //       serial 59.3 ms   5 lanes 32.9 ms (1.80x)   simd 17.6 ms (3.36x)
            //
            // simd is the site's form because it wins the site, not because it
            // wins everywhere: comoment2 genuinely prefers the lanes by 1.22x,
            // and that is given up on purpose. What buys it back is that the
            // lanes are worth NOTHING at the gemv fiber (0.96x -- the SLP
            // permute storm documented in `genReduceComputeBinding`) and only
            // half as much at three streams, so across the site simd is 2.99x
            // geometric mean against the lanes' 1.86x, and it never regresses.
            //
            // NOTE the two 2-stream shapes DISAGREE (gemv wants simd, comoment2
            // wants lanes), which is what rules out splitting this site by
            // operand count: the stream count does not predict the winner, so a
            // per-arity rule here would be a fit to two points, not a principle.
            // `laneCountForStreams` still governs the fallback arm below (and
            // Path B's chunked fold), which is why the arity-aware count
            // survives the change.
            if simdReducibleElem elemStr then
                let simdStmts = fpReassocSimdStmts "+" "__ps" "__pt" "0" "__pn" [] prodAt
                let body =
                    aliasDecls
                    @ [ $"const size_t __pn = {boundOn};"
                        $"{elemStr} __ps = 0;" ]
                    @ simdStmts
                    @ [ "return __ps;" ]
                $$"""[&]() { {{(String.concat " " body)}} }()"""
            else
            // ARITY-AWARE lane count. One lane iteration of an L-operand
            // prodsum keeps L loaded values plus its accumulator live, so the
            // register budget the 8 lanes were measured against is spent L
            // times over: 8 lanes on the THREE-operand form (the comoment3
            // fiber kernel) measured 2.2-2.7x SLOWER than the serial chain
            // while the same 8 lanes helped the two-operand form. The budget
            // rule in `laneCountForStreams` is what reconciles those two
            // measurements; the operand count IS the stream count here.
            let kLanes = laneCountForStreams (List.length argStrs)
            let (laneStmts, resultLane) =
                fpReassocLaneStmts kLanes elemStr "__pl" "__pt" "0" "__pn" prodAt
                    (fun acc rhs -> $"{acc} += {rhs};")
            let shortFallback =
                // Below K elements there is nothing to interleave, so this is
                // the serial chain verbatim -- same seed (the additive
                // identity), same ascending order, hence the same double.
                $"""if (__pn < (size_t){kLanes}) {{ {elemStr} __ps = 0; for (size_t __pt = 0; __pt < __pn; __pt++) {{ __ps += {(prodAt "__pt")}; }} return __ps; }}"""
            let body =
                aliasDecls
                @ [ $"const size_t __pn = {boundOn};"; shortFallback ]
                @ laneStmts
                @ [ $"return {resultLane};" ]
            $$"""[&]() { {{(String.concat " " body)}} }()"""
    | IRDisplayEmit (head, quoted, dataExpr, metaTail, None) ->
        // One display-frame line on stdout (docs/display-frames.md), answering
        // bool. The head / quoting flag / meta tail are elaboration-time
        // constants; only the payload is computed here. The helper is
        // Blade.Display.Frame.cppRuntime's MIRROR of Frame.emit -- keep the
        // two in step or the differential gate says so.
        sprintf "blade_display::emit(%s, %s, %s, %s, %s)"
            (escapeStringLit head)
            (if quoted then "true" else "false")
            (exprToCppCore subst names dataExpr)
            (escapeStringLit metaTail)
            (escapeStringLit Blade.Display.Frame.SessionTag)
    | IRDisplayEmit (head, quoted, dataExpr, metaTail, Some idExpr) ->
        // display.emit_id: same line, with the runtime id where the
        // `<tag><ordinal>` goes. No session tag argument -- the id IS the
        // identity -- and the helper leaves the ordinal counter alone, exactly
        // as Frame.emitId does. There is no sink in this lane and none is
        // needed: a compiled program's frames already reach stdout as it runs.
        sprintf "blade_display::emit_id(%s, %s, %s, %s, %s)"
            (escapeStringLit head)
            (if quoted then "true" else "false")
            (exprToCppCore subst names dataExpr)
            (escapeStringLit metaTail)
            (exprToCppCore subst names idExpr)
    | IRDisplayJson (rank, dataExpr) ->
        // JSON text of a rank-1/rank-2 numeric array. The helper renders each
        // float at its shortest round-trip digits (blade_display::jsonfloat),
        // and the interpreter's CppFormat.formatFloatShortest mirror gives
        // byte parity.
        $"blade_display::json{rank}({(exprToCppCore subst names dataExpr)})"
    | IRDisplayNum dataExpr ->
        $"blade_display::jsonnum({(exprToCppCore subst names dataExpr)})"
    | IRDisplayStr dataExpr ->
        // A user string as a quoted, escaped JSON string. Shares the frame
        // escape table with the quoted-payload path (Frame.jsonString).
        $"blade_display::jsonstr({(exprToCppCore subst names dataExpr)})"
    | IRContains (arrExpr, valueExpr) ->
        // Linear-scan membership test as an IIFE returning bool.
        let arrStr = exprToCppCore subst names arrExpr
        let valStr = exprToCppCore subst names valueExpr
        // A rank-1 compound operand (filtered set) scans its compact buffer:
        // bound = cardinality, elements via .data[i]. Dense stays .extents/[].
        let isR1Compound =
            match inferExprType arrExpr with
            | ArrayElem at -> (isCompoundArrayType at || isSparseArrayType at) && at.IndexTypes.Length = 1
            | _ -> false
        // Dense scan bound goes through the shared literal-or-runtime rule (a
        // compound's cardinality is genuinely dynamic and has no literal form).
        let bound =
            if isR1Compound then $"{arrStr}.idx->cardinality"
            else literalOrRuntimeExtent (inferExprType arrExpr) arrStr 0
        let elemAt = if isR1Compound then $"{arrStr}.data[__ci]" else $"{arrStr}[__ci]"
        $"[&]() {{ for (size_t __ci = 0; __ci < {bound}; __ci++) {{ if ({elemAt} == {valStr}) return true; }} return false; }}()"

    | IRArity (Some n, _) -> string n
    | IRArity (None, paramName) -> 
        // Arity of poly pack - use tuple_size on the named parameter
        $"std::tuple_size_v<std::decay_t<decltype({paramName})>>"
    | IRBind (comp, cont) ->
        // Monadic bind - comp >>= cont
        $"{(exprToCppCore subst names cont)}({(exprToCppCore subst names comp)})"
    | IRReynolds (kernel, isAntisym) ->
        // Reynolds operator wraps kernel
        exprError "reynolds wrapper in expression position"
    | IRZip arrs ->
        // In expression context (e.g. inside a kernel body), zip produces a tuple
        $"""std::make_tuple({(arrs |> List.map (exprToCppCore subst names) |> String.concat ", ")})"""
    | IRStack _ ->
        // Statement-shaped (declares a pool + copy nests, like transpose).
        // Reached only when a stack lands in a bare expression position.
        exprError "stack(...) must be bound to a let before it is used in an expression"
    | IRJoin _ ->
        exprError "join(...) must be bound to a let before it is used in an expression"
    | IRSlice (arr, dim, start, stop) ->
        exprError "slice not yet implemented in codegen"
    | IRCurry (arr, idx, resultRank) ->
        $"{(exprToCppCore subst names arr)}[{(exprToCppCore subst names idx)}]"
    | IRTupleCons (head, tail) ->
        $"std::tuple_cat(std::make_tuple({(exprToCppCore subst names head)}), {(exprToCppCore subst names tail)})"
    | IRTupleDecons tuple ->
        exprToCppCore subst names tuple  // Decons is handled by projection
    | IRMatch (scrutinee, cases) ->
        renderMatchExpr subst names scrutinee cases
    | IRNth -> exprError "nth keyword not supported in expression position"
    | IRZero -> "0"
    | IRPolyIndex (pack, idx) ->
        // For static index, use std::get; otherwise runtime indexing
        match idx with
        | IRLit (IRLitInt n) -> $"std::get<{n}>({(exprToCppCore subst names pack)})"
        | _ -> $"{(exprToCppCore subst names pack)}[{(exprToCppCore subst names idx)}]"
    | IRPolyTail _ ->
        exprError "IRPolyTail should not reach codegen (parameter pack was not monomorphized)"
    | IRParallel (a, b, _) ->
        exprError "parallel combinator in expression position"
    | IRFusion (a, b) ->
        exprError "fusion combinator in expression position"
    | IRChoice (a, b) ->
        // a <|> b: the left operand is bound to a temporary so it is
        // evaluated exactly once (normative scalar <|> semantics, matching
        // the interpreter's Loops.fs/Core.fs choice arms).
        let aStr = exprToCppCore subst names a
        let bStr = exprToCppCore subst names b
        $$"""([&](){ auto __choice_l = {{aStr}}; return __choice_l != 0 ? __choice_l : {{bStr}}; })()"""
    | IRFallback _ ->
        exprError "<|:> (allocated-fallback) in expression position -- it combines whole arrays; bind it and materialize with |> compute"
    | IRGuard (cond, body) ->
        // guard(p, c) -> p ? c : 0 (type-appropriate zero)
        let condStr = exprToCppCore subst names cond
        let bodyStr = exprToCppCore subst names body
        let zeroStr =
            match inferExprType body with
            | IRTScalar ETBool -> "false"
            | IRTScalar ETInt64 | IRTScalar ETInt32 -> "0L"
            | IRTIdxTagged (IRTScalar (ETInt64 | ETInt32), _) -> "0L"
            | _ -> "0.0"
        $"({condStr} ? {bodyStr} : {zeroStr})"
    | IRCompose (f, g) ->
        // f >> g = [&](auto... args) { return g(f(args...)); }
        let fStr = exprToCppCore subst names f
        let gStr = exprToCppCore subst names g
        $$"""[&](auto... __args) { return {{gStr}}({{fStr}}(__args...)); }"""
    | IRComposeObj (f, g) ->
        exprError "compose_obj in expression position"
    | IRComposeMeth (f, g) ->
        exprError "compose_meth in expression position"
    | IRArrayProduct (a, b) ->
        exprError "array_product in expression position"
    | IRFunctorMap (f, c) ->
        exprError "functor_map in expression position"
    | IRConstraintCheck (cond, blCode, message, span) ->
        // Expression-position fallback: a portable IIFE so the guard still
        // fires if it lands somewhere other than a statement slot.
        $"([&](){{ if (!({(exprToCppCore subst names cond)})) {{ blade_rt::panic(\"{blCode}\", \"{message}\", {(panicSpanArgs span)}); }} return 0; }})()"
    | IRBreakIf _ ->
        // A C++ `break` cannot live inside an expression (the IIFE fallback
        // would bind it to no loop) -- statement position only, by refusal.
        exprError "break_if (a rec-array while guard) in expression position"
    | IRAssign (target, value) ->
        let targetStr =
            match target with
            | LVVar id -> Map.tryFind id names |> Option.defaultValue ($"__v{id}")
            | LVIndex (arr, idxs) ->
                let arrStr = exprToCppCore subst names arr
                let idxStr = idxs |> List.map (fun i -> $"[{(exprToCppCore subst names i)}]") |> String.concat ""
                $"{arrStr}{idxStr}"
            | LVField (obj, f) -> $"{(exprToCppCore subst names obj)}.{f}"
            | LVOther e -> exprError "invalid assignment target"
        // Copy-in-place: a sole-owner mut keeps ONE pool for the whole program. The
        // wrapper is NOT repointed at the RHS; the RHS's elements are copied
        // into the pool the mut has always owned, which leaves the RHS temp
        // iteration-owned (its own scope frees it). `pool_base` is rank-generic
        // -- at rank 1 the leaf arm just returns `&data[0]`, i.e. `data` itself.
        match copyInPlaceAssign target value with
        | Some (_, rid, n) ->
            let rhsStr = Map.tryFind rid names |> Option.defaultValue ($"__v{rid}")
            $"std::copy_n(pool_base({rhsStr}.data), {n}, pool_base({targetStr}.data))"
        | None ->
            $"{targetStr} = {(exprToCppCore subst names value)}"
    | IRForRange (vid, lo, hi, body) ->
        exprError "for-range loop in expression position"
    | IROpaqueExtent ->
        // IROpaqueExtent is a marker that lives inside an IRIndexType.Extent
        // slot; it should never reach exprToCpp directly. If it does, the
        // loop builder failed to substitute the surrounding context for the
        // sub-array binding (the ExtentArrayRef path). Surface a visible
        // error rather than silently emit the wrong value.
        exprError "opaque-extent marker reached expression rendering -- kernel-param sub-array was not bound to a concrete extent at the peel point (codegen routing bug)"
    | other ->
        // A hole in the back end, not a refusal -- see `recordUnhandledIRNode`.
        recordUnhandledIRNode "expression position" (other.GetType().Name)
        exprError ($"unsupported IR node: {(other.GetType().Name)}")

/// Generate inline combinator application as an IIFE expression, for `L <@> f`
/// in expression context (not a let RHS or function-body return -- those route
/// through the statement-form genApplyCombinator, which handles every shape).
/// LIMITATION: only the 2-array Cartesian-sum-reduce shape is supported inline;
/// other shapes emit a BLADE_CODEGEN_ERROR sentinel. Fix would be a thin wrapper
/// around genApplyCombinator's statement output (`[&]() { <statements>; return
/// <name>; }()`), deferred since no current test exercises it.

and renderIndexExpr (subst: SubstMap) (names: Map<IRId, string>) arr indices : string =
    let arrStr = exprToCppCore subst names arr
    // Lazy sign-on-read: a compact-group (Symmetric/Antisymmetric/Hermitian,
    // arity >= 2) index whose SOLE slot is fully covered by the indices may be
    // NON-CANONICAL (e.g. A[5][2] on an upper-triangular store). Emit
    // fold-fetch-transform instead of a raw subscript: sort the index tuple
    // (tracking parity), detect implicit-zero (strict diagonal), left-justify
    // to storage coords, fetch, apply the class's read transform (identity /
    // negate-on-swap / conjugate-on-swap) via IIndexTypeBehavior. Fires ONLY
    // for compact-group random access; iteration reads (canonical by
    // construction) keep the raw subscript to stay zero-overhead.
    let rawSubscript () =
        let idxStr = indices |> List.map (fun i -> $"[{(exprToCppCore subst names i)}]") |> String.concat ""
        $"{arrStr}{idxStr}"
    // OrbIdx (depth >= 2) subscript -- docs/plan-orbidx-decompaction.md section 2's
    // read at an ARBITRARY raw tuple:
    //     dense[t] = 0                                if canon(t) is zero-set
    //              = chi(t) * pool[orb_rank(canon(t))] otherwise
    // Emitted as an instantiation of `orb_read<T, Levels...>`, NOT an extension
    // of `canon_fold`: canon_fold<r> is ONE flat sort with no per-level-fold
    // spelling (sorting all prod(ri) coords together maps distinct orbits onto
    // one canonical tuple with a meaningless parity -- the PRODUCT of per-level
    // signs is `orb_canon`'s job). orb_read is already written and CHECKED
    // (`blade test orbwreath`/`orbrank`), and the levels are compile-time
    // constants, so instantiating it is free. The pool is a bare `T*`
    // (genWreathApply's `new T[cells]`), exactly orb_read's first parameter.
    //
    // Emits ONLY for a sole wreath slot at FULL arity, RAISES for every other
    // shape (a None would fall through to `rawSubscript`, emitting `W[i][j]`
    // into a flat pool -- a plausible address, silently wrong). TypeCheck
    // refuses every other shape, so reaching here is an internal routing break.
    let wreathRead () : string option =
        match inferExprType arr with
        | ArrayElem arrTy when arrTy.IndexTypes |> List.exists (fun ix -> ix.Symmetry = SymWreath) ->
            let bail (why: string) =
                let ix = arrTy.IndexTypes |> List.find (fun ix -> ix.Symmetry = SymWreath)
                raise (Blade.Diagnostics.BladeDiagnosticException
                        (Blade.Diagnostics.Codes.iceCodegen
                            ($"internal: a wreath subscript reached codegen in an unsupported shape ({why}) -- OrbIdx{(ppOrbitLevels (orbitLevelsOf ix))} spans {(max 1 ix.Rank)} raw axes and takes exactly that many flat coordinates; TypeCheck should have refused this")))
            (match arrTy.IndexTypes with
             | [ ix ] ->
                 let axes = max 1 ix.Rank
                 if indices |> List.exists (_.IsIRTuple) then
                     bail "a tuple index"
                 elif indices.Length <> axes then
                     bail ($"{indices.Length} coordinates")
                 else
                     let elemStr = elemTypeToCpp arrTy.ElemType
                     let levelArgs = orbLevelArgs (orbitLevelsOf ix)
                     let nStr = exprToCppCore subst names (orbitBaseExtent ix)
                     let coordBuf = "__orbrd"
                     let coordInit =
                         indices
                         |> List.map (fun i -> $"(int)({(exprToCppCore subst names i)})")
                         |> String.concat ", "
                     Some ($"([&]() -> {elemStr} {{ int {coordBuf}[{axes}] = {{ {coordInit} }}; return orbit_wreath_utilities::orb_read<{elemStr}, {levelArgs}>({arrStr}, {coordBuf}, (int)({nStr})); }}())")
             | _ -> bail "a wreath group combined with other index slots")
        | _ -> None
    let lazyCompactRead () : string option =
        match inferExprType arr with
        | ArrayElem arrTy ->
            // Generalized lazy read: the index-type list may contain multiple
            // compact groups (e.g. an interior antisym decompact result
            // AntisymIdx<2> -> Idx -> AntisymIdx<2>) interleaved with plain
            // freed slots. Each compact group folds INDEPENDENTLY (own
            // canon_fold/zero-guard/left-justify/transform); plain slots pass
            // through. Fires only when at least one slot is compact AND the
            // indices fully cover the index list; otherwise raw subscript.
            let slots = arrTy.IndexTypes
            let totalRank = slots |> List.sumBy (fun s -> max 1 s.Rank)
            let anyCompact = slots |> List.exists (fun s -> s.Symmetry <> SymNone && (max 1 s.Rank) >= 2)
            if anyCompact && indices.Length = totalRank then
                let elemTypeStr = irTypeToCpp arrTy.ElemType
                let idxStrs = indices |> List.map (fun i -> exprToCppCore subst names i) |> Array.ofList
                // Walk slots, consuming arity indices each. For compact groups
                // emit fold-locals; collect (fetchSubParts, transformChain).
                let sb = System.Text.StringBuilder()
                let mutable cursor = 0
                let mutable groupNum = 0
                let mutable fetchParts = []      // C++ subscript pieces in slot order
                let mutable transforms = []      // (parityVar, tfStr) per compact group
                let mutable ok = true
                for s in slots do
                    let a = max 1 s.Rank
                    let these = [ for j in 0 .. a - 1 -> idxStrs.[cursor + j] ]
                    cursor <- cursor + a
                    if s.Symmetry <> SymNone && a >= 2 then
                        let beh = behaviorFor s.Symmetry
                        let strictArg =
                            match beh.Canonicalize () with
                            | CanonSortStrict -> "true"
                            | CanonSort | CanonNone -> "false"
                            // canon_fold<r> is ONE flat sort with a strict flag;
                            // it has no spelling for a per-level fold, and
                            // emitting "false" here would silently canonicalize
                            // a wreath tuple as a plain multiset. The real read
                            // is `wreathRead` above (an `orb_read`
                            // instantiation), which runs FIRST; reaching here
                            // means a wreath slot appeared in a shape that read
                            // declines -- combined with other slots, or at the
                            // wrong arity -- which TypeCheck refuses. Backstop.
                            | CanonWreathFold ->
                                failwith (orbitStorageUnsupported "lazy compact read (canon_fold emission)"
                                                                  (orbitLevelsOf s))
                        let tf =
                            match beh.ReadTransform () with
                            | TfIdentity -> "nested_array_utilities::ReadTransform::Identity"
                            | TfNegateOnSwap -> "nested_array_utilities::ReadTransform::NegateOnSwap"
                            | TfConjugateOnSwap -> "nested_array_utilities::ReadTransform::ConjugateOnSwap"
                        let g = groupNum
                        groupNum <- groupNum + 1
                        sb.Append($$"""std::array<size_t,{{a}}> __g{{g}} = { {{(String.concat ", " these)}} }; """) |> ignore
                        sb.Append($"bool __z{g}; int __p{g} = nested_array_utilities::canon_fold<{a}>(__g{g}, {strictArg}, __z{g}); ") |> ignore
                        sb.Append($"if (__z{g}) return {elemTypeStr}(); ") |> ignore
                        sb.Append($"auto __c{g} = nested_array_utilities::canon_left_justify<{a}>(__g{g}, {strictArg}); ") |> ignore
                        for j in 0 .. a - 1 do
                            fetchParts <- fetchParts @ [ $"[__c{g}[{j}]]" ]
                        transforms <- transforms @ [ ($"__p{g}", tf) ]
                    elif s.Symmetry <> SymNone && a = 1 then
                        // arity-1 compact (e.g. SymIdx<1> = Idx): no fold.
                        fetchParts <- fetchParts @ [ $"[{these.[0]}]" ]
                    else
                        // plain slot(s): pass each index through directly.
                        for t in these do fetchParts <- fetchParts @ [ $"[{t}]" ]
                if not ok then None
                else
                    let fetch = String.concat "" fetchParts
                    // chain transforms: v0 = transform(base, p0); v1 = transform(v0,p1); ...
                    let body = System.Text.StringBuilder()
                    body.Append(sb.ToString()) |> ignore
                    body.Append($"{elemTypeStr} __v = {arrStr}{fetch}; ") |> ignore
                    match transforms with
                    | [] ->
                        // No real fold happened (shouldn't reach: anyCompact true) -- return raw.
                        body.Append("return __v;") |> ignore
                    | _ ->
                        let mutable prev = "__v"
                        transforms |> List.iteri (fun i (pv, tf) ->
                            let outv = $"__tv{i}"
                            body.Append($"{elemTypeStr} {outv} = nested_array_utilities::canon_transform<{elemTypeStr}>({prev}, {pv}, {tf}); ") |> ignore
                            prev <- outv)
                        body.Append($"return {prev};") |> ignore
                    Some ($$"""([&]() -> {{elemTypeStr}} { {{(body.ToString())}} }())""")
            else None
        | _ -> None
    // Compound tuple indexing (formalism 4.5): when `arr` is a Compound<T,RANK>
    // and the first index is a tuple, the tuple's coords gather through the
    // compound's linearize rather than a peel chain.
    //   full (j = k): no trailing -> arr({coords}) scalar; trailing all given
    //     -> arr({coords}, trail) scalar; trailing remain -> arr.row({coords})
    //     T* sub-view.
    //   partial (j < k): reconstituted by one of four runtime helpers
    //     (window / gather x dense / compound), dispatched on prefix-ness and
    //     residual rank -- see CompoundPartial. Trailing dims + partial read
    //     stays gated (hard error).
    let compoundRead () : string option =
        match inferExprType arr with
        | ArrayElem arrTy when isCompoundArrayType arrTy || isSparseArrayType arrTy ->
            // Tabulated-head read: CompoundIdx and SparseIdx share the tuple
            // application form and the full-read emission; they differ ONLY in
            // the partial-index reconstitution (compound has the zero-copy
            // prefix/window family; sparse is always a gather).
            let isSparse = isSparseArrayType arrTy
            let headKind ix = ix.IxKind = IxKCompound || ix.IxKind = IxKSparse
            // Rank-1 tabulated scalar sugar: `C(i)` on a rank-1 head
            // (the filtered-set case) is the 1-tuple read `C((i))` --
            // there is no way to even WRITE a 1-tuple literal at the
            // surface, so the scalar spelling is the canonical one.
            // Normalize to the tuple path; without this it fell to the
            // raw-subscript peel (`C[i]`), which Compound cannot compile.
            let k1 =
                arrTy.IndexTypes
                |> List.tryFind headKind
                |> Option.map (_.Rank)
            let indices =
                match k1, indices with
                | Some 1, first :: rest when (not first.IsIRTuple) ->
                    IRTuple [first] :: rest
                | _ -> indices
            match indices with
            | (IRTuple coords) :: trailingIdxs ->
                let k =
                    arrTy.IndexTypes
                    |> List.tryFind headKind
                    |> Option.map (_.Rank)
                    |> Option.defaultValue coords.Length
                let trailingDims =
                    match arrTy.IndexTypes with
                    | _ :: rest -> rest
                    | [] -> []
                match classifyCompoundIndexTuple k coords with
                | CompoundPartial (pinned, freePos) ->
                    // Partial (wildcard/short-tuple) indexing is a SPARSE-only
                    // feature: sparse partials are ALWAYS gathers (the key
                    // table is insertion-ordered, no lex-sorted contiguity for
                    // a prefix window), keyed on residual rank + trail. A
                    // COMPOUND head can no longer produce a partial tuple
                    // (validateTabulatedIndex rejects the forms), so reaching
                    // the compound arm here is an internal invariant break.
                    let j = pinned.Length
                    let residualRank = freePos.Length
                    if not isSparse then
                        raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.backendLimit Blade.Ast.noSpan "internal: a partial compound index tuple reached codegen -- partial/wildcard reads on CompoundIdx were removed (use SparseIdx); the typecheck flat-subscript packing should have made this unreachable"))
                    elif not (List.isEmpty trailingIdxs) then
                        raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.backendLimit Blade.Ast.noSpan ("Partial sparse indexing combined with a SUPPLIED trailing index is not yet supported; leave the trailing dim free (omit it or write `_`), or index the residual separately (let r = S((...)); r(...)).")))
                    elif trailingDims.Length > 1 then
                        raise (Blade.Diagnostics.BladeDiagnosticException (Blade.Diagnostics.Codes.backendLimit Blade.Ast.noSpan ($"Partial sparse indexing with {trailingDims.Length} trailing dimensions is not supported (multi-trailing is unsupported throughout: the wrapper stores only the trailing-stride product, not per-dim extents).")))
                    else
                        let hasTrail = not (List.isEmpty trailingDims)
                        let elemStr = elemTypeToCpp arrTy.ElemType
                        // (size_t) casts: coordinate exprs are int64-typed at
                        // the Blade level; a bare int64 VARIABLE inside a
                        // std::array<size_t,J> brace-init is a narrowing
                        // error (literals are exempt as constant exprs).
                        let pinnedVals = pinned |> List.map (fun (_, c) -> $"(size_t)({(exprToCppCore subst names c)})")
                        let pinnedArr = $"""std::array<size_t, {j}>{{{(String.concat ", " pinnedVals)}}}"""
                        let posArr =
                            $"""std::array<size_t, {j}>{{{(pinned |> List.map (fst >> string) |> String.concat ", ")}}}"""
                        if residualRank >= 2 then
                            Some ($"nested_array_utilities::make_partial_sparse_gather<{elemStr}, {k}, {j}>({arrStr}, {pinnedArr}, {posArr})")
                        else
                            let fn = if hasTrail then "make_sparse_gather_dense_trail" else "make_sparse_gather_dense"
                            Some ($"nested_array_utilities::{fn}<{elemStr}, {k}, {j}>({arrStr}, {pinnedArr}, {posArr})")
                | CompoundFull ->
                    // j = k: full index. (size_t) casts are needed because an
                    // int64-typed coordinate VARIABLE (e.g. a lifted-lambda
                    // param) is a narrowing error in a std::array<size_t>
                    // brace-init (literals and size_t loop vars are exempt).
                    let coordStrs = coords |> List.map (fun c -> $"(size_t)({(exprToCppCore subst names c)})")
                    let coordArr = $"""std::array<size_t, {k}>{{{(String.concat ", " coordStrs)}}}"""
                    if List.isEmpty trailingDims then
                        // No trailing dims: scalar cell.
                        Some ($"{arrStr}({coordArr})")
                    elif trailingIdxs.Length >= trailingDims.Length then
                        // Trailing dims fully supplied: scalar via operator()
                        // with the (single) trailing offset. Multi-trailing is
                        // not yet supported (trailing_stride is a product, not
                        // per-dim), matching the rest of the compound codegen;
                        // the first trailing index is the offset.
                        let trailStr =
                            match trailingIdxs with
                            | t :: _ -> exprToCppCore subst names t
                            | [] -> "0"
                        Some ($"{arrStr}({coordArr}, {trailStr})")
                    else
                        // Trailing dims remain unindexed: sub-view base pointer.
                        // Any partially-supplied trailing indices then subscript
                        // the returned T* in slot order.
                        let restSubs =
                            trailingIdxs
                            |> List.map (fun i -> $"[{(exprToCppCore subst names i)}]")
                            |> String.concat ""
                        Some ($"{arrStr}.row({coordArr}){restSubs}")
            | _ -> None  // compound array but first index isn't a tuple (shouldn't reach: TypeCheck enforces the tuple form)
        | _ -> None
    // Plain dense PARTIAL positional read in EXPRESSION position: `A(i)` with
    // fewer subscripts than the rank denotes a row/slab sub-view, but the raw
    // subscript `A[i]` is `operator[]` -> `data[i]`, a bare pointer that has
    // lost `.extents` -- a type error or silently shape-less value wherever the
    // residual is consumed as an array. Emit the same sub-view aggregate the
    // BINDING path builds (densePartialSubview) as a prvalue instead: data
    // steps through the consumed leading dims, extents shifts past them.
    // Same scoping as the binding path -- fully plain-dense rectangular
    // (IxKPlain/SymNone/arity-1); compound reads go through
    // compoundRead above, and ragged/packed-symmetric row pointers must NOT be
    // re-wrapped (their axes fail the predicate).
    //
    // Deliberately NOT gated on `ix.Kind = SDimension`: an index record's Kind
    // is a statement about ONE apply (S = this apply's grid iterates it, T =
    // this apply's kernel contributed it), never about the value's storage --
    // see reSDimOperand's doc in TypeCheck. An array a kernel RETURNED carries
    // inherited TDimension stamps, and its rows are exactly as rectangular as
    // anyone else's; gating on Kind sent `G(j)` of such a G (the leading-axis
    // fold's step slice) to the raw `data[j]` pointer, which then failed g++ in
    // every consuming position (call argument, apply operand, let binding).
    // A CONSUMED (leading, subscripted-through) axis may also be GROUP-OUTER:
    // the rectangular grid a grouped apply returns is a dense pool whose
    // extents[0] is the group count, so `data[j]` / `extents + 1` are exact.
    // RESIDUAL axes must stay plain -- a surviving ragged member axis means
    // the row shapes differ and the aggregate would lie.
    let densePartialSubviewExpr () : string option =
        if List.isEmpty indices
           || indices |> List.exists (_.IsIRTuple) then None
        else
            match inferExprType arr with
            | ArrayElem arrTy
                    when arrTy.IndexTypes.Length > indices.Length
                         && arrTy.IndexTypes |> List.mapi (fun d ix -> (d, ix))
                            |> List.forall (fun (d, ix) ->
                                (ix.IxKind = IxKPlain
                                 || (d < indices.Length && ix.IxKind = IxKGroupOuter))
                                && ix.Symmetry = SymNone
                                && ix.Rank = 1) ->
                let residTy = { arrTy with IndexTypes = List.skip indices.Length arrTy.IndexTypes }
                let subscripts =
                    indices
                    |> List.map (fun i -> $"[{(exprToCppCore subst names i)}]")
                    |> String.concat ""
                Some ($"{(cppArrayTypeStr residTy)}{{ {arrStr}.data{subscripts}, {arrStr}.extents + {indices.Length} }}")
            | _ -> None
    match compoundRead () with
    | Some code -> code
    | None ->
        // The wreath read runs BEFORE the compact fold: a wreath slot passes
        // lazyCompactRead's `anyCompact` test (it IS packed) and would reach
        // canon_fold's per-level backstop, which has no spelling for it.
        (match wreathRead () with
         | Some code -> code
         | None ->
            match lazyCompactRead () with
            | Some code -> code
            | None ->
                match densePartialSubviewExpr () with
                | Some code -> code
                | None -> rawSubscript ())


and renderMatchExpr (subst: SubstMap) (names: Map<IRId, string>) scrutinee cases : string =
    // A match compiles to a chain of ternaries, one arm each --
    // `(test ? bound : rest)` -- bottoming out in the non-exhaustive abort.
    // What each arm contributes comes from ONE recursive pattern compiler
    // (`patCompile`) rather than a per-constructor emitter, which is exactly
    // what makes NESTED patterns work: the emitter this replaced only ever
    // looked one level down, so `((a, b), c)` bound nothing and rendered its
    // body against dangling names, `head :: tail` fell into a catch-all that
    // did the same, and a struct pattern read a real C++ struct with
    // `std::get<i>`. All three typechecked cleanly and died at g++ -- and the
    // interpreter handled all three correctly, so each was also a live twin
    // divergence.
    let scrut = exprToCppCore subst names scrutinee
    let scrutTy = inferExprType scrutinee
    // The non-exhaustive panic IIFE, typed as the MATCH RESULT rather than a
    // hardcoded double: as a ternary operand it participates in the chain's
    // common type, so a double-typed abort on an Int64-valued match poisoned
    // the whole chain to double and died as a g++ -Werror=float-conversion --
    // making BL8002 unreachable for every non-double element type. `return
    // {};` value-initializes whatever the type is (the panic never returns;
    // C++ just needs the statement).
    let abortExpr =
        let retTyStr =
            match cases |> List.tryHead with
            | Some c ->
                (match inferExprType c.Body with
                 | ArrayElem arr -> cppArrayTypeStr arr
                 | IRTInfer _ -> "double"
                 | t -> irTypeToCpp t)
            | None -> "double"
        $$"""([&]() -> {{retTyStr}} { blade_rt::panic("BL8002", "Blade: non-exhaustive match", nullptr, 0); return {}; }())"""

    let mergeParts (parts: (string list * (IRId * string * string) list) list) =
        (parts |> List.collect fst, parts |> List.collect snd)

    /// The `n` slot accesses of one access, each with its own type where one
    /// is recoverable. A struct is read by declared field NAME -- `std::get<i>`
    /// on a real C++ struct is not valid C++, which is how a struct pattern
    /// used to reach g++.
    let slotsOf (access: string) (ty: IRType option) (n: int) : (string * IRType option) list =
        match ty |> Option.bind IR.tryLookupStructFields with
        | Some flds when flds.Length = n ->
            flds |> List.map (fun (fn, t) -> ($"{access}.{fn}", Some t))
        | _ ->
            match ty with
            | Some (IRTTuple ts) when ts.Length = n ->
                ts |> List.mapi (fun i t -> ($"std::get<{i}>({access})", Some t))
            | _ -> [ for i in 0 .. n - 1 -> ($"std::get<{i}>({access})", None) ]

    /// Compile ONE pattern against a C++ access expression whose IR type is
    /// `ty` (`None` where the type is not recoverable at that depth) into
    ///
    ///   - `tests` -- boolean conjuncts, all of which must hold, in
    ///     short-circuit order: a variant's tag test is emitted BEFORE any
    ///     conjunct that reads its payload, so `&&` keeps the payload read on
    ///     the alternative that actually holds it;
    ///   - `binds` -- `(id, name, initializer)` declarations, in order, run
    ///     only once every test has passed.
    ///
    /// Composition over sub-patterns is the whole point; the three shapes
    /// this file used to get wrong are just its three recursive cases. Only
    /// three decisions consult `ty` at all -- tuple-vs-struct slot access, a
    /// cons tail's arity, and the type handed to the next level down -- and a
    /// struct PATTERN carries its own type name, so it needs none of them.
    let rec patCompile (access: string) (ty: IRType option) (pat: IRPattern)
            : string list * (IRId * string * string) list =
        match pat with
        | IRPatWild -> ([], [])
        | IRPatVar id -> ([], [ (id, $"__match_{id}", access) ])
        | IRPatLit lit -> ([ $"{access} == {(litToCpp lit)}" ], [])
        | IRPatStruct (typeName, fieldPats) ->
            // BY NAME. The declared field list is consulted only to TYPE the
            // sub-pattern; the access itself is `.field`, so a pattern that
            // lists fields out of declaration order still reads the right
            // ones -- positional lowering used to bind every one of them to
            // the wrong slot.
            let declared = IR.tryLookupStructFieldsByName typeName
            fieldPats
            |> List.map (fun (fname, fpat) ->
                let fty =
                    declared
                    |> Option.bind (List.tryFind (fun (n, _) -> n = fname))
                    |> Option.map snd
                patCompile $"{access}.{fname}" fty fpat)
            |> mergeParts
        | IRPatTuple pats -> patCompileSlots (slotsOf access ty pats.Length) pat
        | IRPatCons _ ->
            // TUPLES ONLY, deliberately -- the interpreter's cons arm matches
            // `VTuple` and nothing else, so a struct scrutinee here must fall
            // through to the next arm rather than get decomposed by declared
            // field order. `slotsOf` would happily hand back those fields.
            match ty with
            | Some (IRTTuple ts) when ts.Length >= 2 ->
                patCompileSlots (slotsOf access ty ts.Length) pat
            | _ ->
                // A parameter pack still spelled as one at codegen was never
                // specialized (`let head :: tail = A` is the shape the
                // monomorphizer knows), and a one-slot or non-tuple scrutinee
                // has no remainder to bind. Refuse by name rather than emit a
                // body over dangling identifiers, which is what the
                // pre-rewrite catch-all did.
                ([ exprError "cons pattern (head :: tail) in a match needs a tuple scrutinee with at least two slots; destructure a parameter pack with a let binding instead" ], [])
        | IRPatVariant (ctorName, _, innerOpt, isEnum) ->
            let test =
                if isEnum then $"{access} == {ctorName}"
                else $"std::holds_alternative<{ctorName}_T>({access})"
            match innerOpt with
            | None -> ([ test ], [])
            | Some inner ->
                // The tag test leads, so the payload read below it is
                // short-circuited away on every other alternative.
                let (ts, bs) = patCompile $"std::get<{ctorName}_T>({access}).value" None inner
                (test :: ts, bs)

    /// Compile a pattern against a RUN of slots rather than a single access.
    /// Both the tuple arm and a cons TAIL land here, which is what keeps
    /// `h :: (a, b)` reading `std::get<1>` and `std::get<2>` of the original
    /// instead of rebuilding an intermediate tuple and immediately taking it
    /// apart again (once per leaf, nested -- an exponential in cons depth).
    and patCompileSlots (slots: (string * IRType option) list) (pat: IRPattern)
            : string list * (IRId * string * string) list =
        match pat with
        | IRPatWild -> ([], [])
        | IRPatTuple pats when pats.Length = slots.Length ->
            List.zip slots pats
            |> List.map (fun ((a, t), p) -> patCompile a t p)
            |> mergeParts
        | IRPatCons (headPat, tailPat) when slots.Length >= 2 ->
            let (headAcc, headTy) = slots.Head
            mergeParts [ patCompile headAcc headTy headPat
                         patCompileSlots slots.Tail tailPat ]
        | _ ->
            // The pattern wants the run as ONE value (a binder, a literal, a
            // variant). Blade has no 1-tuple -- `(x)` IS `x` -- so a
            // single-slot run is its bare element; that is the same rule
            // `let head :: tail` applies (Lowering's `subBindingValue`) and
            // the one TypeCheck types the leaf by.
            match slots with
            | [ (a, t) ] -> patCompile a t pat
            | _ ->
                let acc =
                    $"""std::make_tuple({(slots |> List.map fst |> String.concat ", ")})"""
                let ty =
                    if slots |> List.forall (fun (_, t) -> Option.isSome t)
                    then Some (IRTTuple (slots |> List.map (snd >> Option.get)))
                    else None
                patCompile acc ty pat

    /// One arm, given the code for everything after it. `rest` appears twice
    /// when an arm has BOTH a refutable pattern and a guard (either failure
    /// falls through to the same place); the emitter this replaced had the
    /// same shape, and match chains are short.
    let renderArm (case: IRMatchCase) (restStr: string) : string =
        let (tests, allBinds) = patCompile scrut (Some scrutTy) case.Pattern
        let binds =
            match case.Pattern with
            | IRPatVar id ->
                // Economy preserved byte-for-byte from the emitter this
                // replaced: a TOP-LEVEL binder nobody reads emits neither a
                // declaration nor the IIFE that would carry it, so `| x -> 1`
                // stays the bare body it has always been.
                let used =
                    (collectVarRefsIR case.Body).Contains id
                    || (case.Guard
                        |> Option.map (fun g -> (collectVarRefsIR g).Contains id)
                        |> Option.defaultValue false)
                if used then allBinds else []
            | _ -> allBinds
        let names' = binds |> List.fold (fun acc (id, name, _) -> Map.add id name acc) names
        let bodyStr = exprToCppCore subst names' case.Body
        // The guard runs AFTER the binders (it may read them) and falls
        // through to the next arm when false -- on the last arm that is the
        // abort. A guard on a variant arm used to be dropped on the floor
        // here, taking the arm unconditionally.
        let guarded =
            match case.Guard with
            | Some g -> $"({(exprToCppCore subst names' g)} ? {bodyStr} : {restStr})"
            | None -> bodyStr
        let bound =
            if List.isEmpty binds then guarded
            else
                let decls =
                    binds |> List.map (fun (_, n, init) -> $"auto {n} = {init}") |> String.concat "; "
                $$"""[&]() { {{decls}}; return {{guarded}}; }()"""
        if List.isEmpty tests then bound
        else $"""({(String.concat " && " tests)} ? {bound} : {restStr})"""

    let rec genCase (cs: IRMatchCase list) : string =
        match cs with
        | [] -> abortExpr
        | case :: rest -> renderArm case (genCase rest)
    genCase cases


and renderReduceExpr (subst: SubstMap) (names: Map<IRId, string>) arrExpr kernelExpr (initExpr: IRExpr option) : string =
    // Inline reduction as an IIFE (mirrors genBinding's loop, wrapped in
    // `[&]() { ... }()` for expression context: kernel bodies, arithmetic).
    // Empty-array guard emits only for dynamic extents (statically-proven
    // nonempty skips it; typecheck rejects statically-empty inputs).
    //
    // The comm-licensed OpenMP fold path deliberately does NOT reach here --
    // the expression form stays serial even for a licensed `where ... omp`
    // kernel, since an IIFE reduce's surrounding context is routinely already
    // inside a parallel region (opening a nested team would be a pessimisation
    // or a wrong team size). The statement forms (genReduceBinding /
    // genReduceComputeBinding) own a whole binding and are what make the
    // parallel region safe; hoist the fold to its own `let` for that shape.
    //
    // The decline above is legitimate; being SILENT about it was not. A kernel
    // carrying `where ... omp` that lands here emits the census marker below,
    // for the same reason every nest-level decline does (`ompSuppressedMarker`):
    // "never asked" and "asked, couldn't honour" were byte-identical, so a user
    // who wrote the clause had no way to tell which happened.
    //
    // This is not a rare corner. Since the 2026-08-09 `reduce` ruling
    // (docs/features/sql.md 10) the DEFAULT is the innermost-axis partial fold,
    // and `partialFold` (TypeCheck) desugars it into the row-wise
    // `method_for(src) <@> lambda(row) -> reduce(row, op, init)` -- which puts
    // the fold in exactly this expression position. So every rank>=2
    // `reduce(A, <omp-licensed kernel>, init)` written without `axes = rank`
    // arrives here, and the marker is what tells its author that the clause
    // bought nothing and `axes = rank` is the spelling that would.
    // A BARE range operand (`reduce(0..n, (+))` in expression position --
    // kernel bodies / function-body returns, where the lift pass's operand
    // hoist does not reach): materialize the iota into an IIFE-local, then
    // re-render the fold over a synthetic named operand carrying the range's
    // array type (IRParam renders as its bare name and typeOf answers the
    // carried type, so every bound/elem decision below just works). Binding
    // and module-scope reduces take the lift road to genRangeBinding instead.
    // Non-plain ranges (compound/sparse/halo/multi-slot) answer None from the
    // materializer and fall through to the ordinary refusal.
    let rangeInline =
        match arrExpr with
        | IRRange ([ ix ], _) ->
            let rname = $"__rng{ix.Id}"
            materializeInlineForm subst names rname (lazy "int64_t") arrExpr
            |> Option.map (fun (stmts, _) -> (rname, ix, stmts))
        | _ -> None
    match rangeInline with
    | Some (rname, ix, stmts) ->
        let arrTy = mkArrayLike { ElemType = IRTScalar ETInt64; IndexTypes = [ix]; IsVirtual = false; Identity = None }
        let inner = renderReduceExpr subst names (IRParam (rname, 0, arrTy)) kernelExpr initExpr
        // Expression position: the materialization is spliced into an IIFE,
        // so there is no statement scope whose exit could carry a free -- the
        // descriptors are dropped (same unchanged pre-existing leak as
        // renderLetExpr's inline-form site).
        $$"""([&]() { {{(stmts |> String.concat " ")}} return {{inner}}; }())"""
    | None ->
    let arrStr = exprToCppCore subst names arrExpr
    let elemType =
        match inferExprType arrExpr with
        | ArrayElem a -> a.ElemType
        | _ -> IRTScalar ETFloat64  // Fallback; typecheck enforces array input
    let elemStr = elemTypeToCpp elemType
    let isStaticallyNonEmpty =
        match inferExprType arrExpr with
        | ArrayElem at when at.IndexTypes.Length >= 1 ->
            match tryEvalIntIR at.IndexTypes.[at.IndexTypes.Length - 1].Extent with
            | Some n -> n > 0L
            | None -> false
        | _ -> false
    // A compound array's present cells live in a flat compact buffer
    // (`.data`, length `.size()` = cardinality * trailing_stride), so a
    // reduction walks that buffer directly -- there is no `.extents` or
    // operator[]. This reduces over ALL present values (the cardinality
    // cells for an all-dims mask, spanning the trailing block for a partial
    // mask). cardinality is a runtime value, so the empty guard always emits.
    let isCompound =
        match inferExprType arrExpr with
        | ArrayElem at -> isCompoundArrayType at || isSparseArrayType at
        | _ -> false
    // A reduce operand can also be a peeled ragged/dep-idx row, which lowers
    // to RaggedRow<T>. RaggedRow exposes its length as `.len` (a bare size_t),
    // NOT `.extents[0]` -- and it is indexed `g[i]` via its operator[]. Detect
    // it so the length bound uses `.len`; the default `%s[%s]` access already
    // works for RaggedRow.
    // Only a RANK-1 ragged/dep-idx operand is a RaggedRow<T> (with an inline
    // `.len`). A rank-2+ Ragged<T> has `.extents`/`.lens`, not `.len`, so it
    // must fall through to the default `.extents[0]`. Same predicate the peel
    // emission and IRExtent use, so the length accessor stays consistent with
    // the operand's actual C++ type.
    let isRagged =
        match inferExprType arrExpr with
        | ArrayElem at -> isRaggedRowType at
        | _ -> false
    let reduceAccAt (i: string) =
        if isCompound then $"{arrStr}.data[{i}]" else $"{arrStr}[{i}]"
    let reduceBound =
        if isCompound then $"({arrStr}.idx->cardinality * {arrStr}.trailing_stride)"
        elif isRagged then $"{arrStr}.len"
        // Dense operand: shared literal-or-runtime rule. Compound cardinality
        // and a RaggedRow's `.len` are genuinely dynamic and stay runtime reads.
        else literalOrRuntimeExtent (inferExprType arrExpr) arrStr 0
    let reduceNonEmpty = isStaticallyNonEmpty && not isCompound && not isRagged
    // Reduce-kernel resolution via `resolveCallable`. The fold kernel emits as
    // a local wrapper closure inside the IIFE; the fold loop invokes the
    // wrapper on `(acc, arr[__ri])`. The wrapper lives inside the IIFE's
    // `[&]() { ... }()` scope, so name collisions across multiple reduces at
    // the same outer scope are structurally avoided -- each IIFE is its own
    // block. Wrappers carry the right signature because `lowerTypedSection`
    // reads a section's resolved type from the typed expression and
    // `inferReduce` unifies the kernel's params with the array element type
    // before zonking (so `reduce(int_array, (+))` doesn't mismatch Float64).
    match resolveCallable kernelExpr with
    | Some callable when callable.Params.Length = 2 ->
        let (wrapperCode, wname) = genCallableWrapper names "" callable
        let wrapperStr = wrapperCode |> String.concat " "
        // Census marker for a dropped `omp` clause (see the note at the top of
        // this function). A BLOCK comment, because every arm below space-joins
        // its statements into ONE line -- a `//` would swallow the fold.
        //
        // Triggered by the REQUEST (`IsOmpParallel`), not by the licence: the
        // marker's claim is "you asked and did not get it", which is true of a
        // licensed and an unlicensed kernel alike. (In practice only licensed
        // ones arrive -- `checkFoldOmpLicense` refuses the rest with BL4016
        // before codegen -- so this is the same population the statement form's
        // `parallelFold.IsSome` selects, reached by the honest predicate.)
        //
        // The reason is STRUCTURAL and identical under BLADE_OMP_THREADS: this
        // site declines in both modes, so naming the knob here would be a lie.
        // That is also what keeps the knob-equivalence pins meaningful -- a
        // program whose only omp site is an expression reduce emits the same
        // text either way.
        let ompNote =
            ompSuppressedBlockMarker callable.IsOmpParallel
                "reduce in expression position (kernel body / inline arithmetic) opens no team -- its context may already be a parallel region; bind the fold to its own `let` (or fold all axes with `axes = rank`) for the parallel form"
        // BLADE_FP_REASSOC, expression form. Same K-lane shape as the statement
        // form, minus the thread chunking: ONE chunk, K lanes, no pragma. That
        // is the right shape here for the reason the note above gives -- an IIFE
        // reduce is routinely already inside a parallel region -- and the lanes
        // need no team, so the answer stays a fixed function of the data and K.
        // Licence is the ordinary one: a recognised builtin body, or declared
        // comm. An unlicensed user kernel stays serial with the knob on.
        //
        // LANES AND NOT `omp simd reduction`, DELIBERATELY: this is the
        // EXPRESSION TWIN of `genReduceBinding`'s unlicensed-serial arm -- the
        // same fold over the same materialized rank-1 array, differing only in
        // where the result lands -- so the two must not be measured separately
        // or chosen separately. that arm's measurement (sumred, both
        // memory-hierarchy regimes; see the note there) says lanes, by 1.12x
        // bandwidth-bound and 1.90x cache-resident. This site inherits it.
        //
        // ONE operand stream, so the lane count is `foldLaneCount` outright:
        // `IRReduce` folds a MATERIALIZED rank-1 array, and its lane iteration
        // keeps exactly one loaded value plus the accumulator live -- the shape
        // `laneCountForStreams`'s anchor was measured on. A prodsum-like
        // multi-stream body never reaches here: it lowers either to the
        // `IRProdSum` IIFE above (which applies the budget rule) or, unforced,
        // to `IRReduceCompute` (which applies it in genReduceComputeBinding).
        let laneForm (loExpr: string) (seedStr: string) (guard: string) =
            let (laneStmts, resultLane) =
                fpReassocLaneStmts foldLaneCount elemStr "__rlane" "__ri" "__rlo" "__rn" reduceAccAt
                    (fun acc rhs -> $"{acc} = {wname}({acc}, {rhs});")
            let stmts =
                [ $"const size_t __rn = {reduceBound};"
                  $"{elemStr} __r = {seedStr};"
                  $"const size_t __rlo = {loExpr};"
                  // `__rn < __rlo` cannot happen once the guard has run, but the
                  // saturating count costs nothing and keeps the short-branch
                  // test from wrapping around if it ever could.
                  "const size_t __rcnt = (__rn > __rlo) ? (__rn - __rlo) : (size_t)0;"
                  $"""if (__rcnt < (size_t){foldLaneCount}) {{ for (size_t __ri = __rlo; __ri < __rn; __ri++) {{ __r = {wname}(__r, {(reduceAccAt "__ri")}); }} return __r; }}""" ]
                @ laneStmts
                @ [ $"__r = {wname}(__r, {resultLane});"
                    "return __r;" ]
            $$"""[&]() { {{ompNote}}{{guard}}{{wrapperStr}} {{(String.concat " " stmts)}} }()"""
        let mayLane = fpReassocEnabled () && foldReorderLicensed callable
        match initExpr with
        | Some initE ->
            // 3-arg form: the accumulator seeds from init and the loop covers
            // ALL elements. The empty fold is defined (it is init), so no
            // emptiness guard is needed for any extent, static or dynamic.
            let initStr = exprToCppCore subst names initE
            if mayLane then laneForm "0" initStr ""
            else
            sprintf "[&]() { %s%s %s __r = %s; for (size_t __ri = 0; __ri < %s; __ri++) { __r = %s(__r, %s); } return __r; }()"
                ompNote wrapperStr elemStr initStr reduceBound wname (reduceAccAt "__ri")
        | None ->
        let guard =
            if reduceNonEmpty then ""
            else $"if ({reduceBound} == 0) {{ blade_rt::panic(\"BL8003\", \"reduce: empty array, no reduction possible\", nullptr, 0); }} "
        if mayLane then laneForm "1" (reduceAccAt "0") guard
        else
        sprintf "[&]() { %s%s%s %s __r = %s; for (size_t __ri = 1; __ri < %s; __ri++) { __r = %s(__r, %s); } return __r; }()"
            ompNote guard wrapperStr elemStr (reduceAccAt "0") reduceBound wname (reduceAccAt "__ri")
    | _ ->
        "/* reduce: non-callable kernel (typechecker or IR bug) */"



and renderLetExpr (subst: SubstMap) (names: Map<IRId, string>) id value body : string =
    // For inline let expressions, we need statement context
    let names' = Map.add id ($"__v{id}") names
    if isUnitExpr value then
        // Unit-valued binding. lowerTypedBlock sequences STATEMENTS
        // (assignments, for-in loops) through dummy lets, so a unit value
        // here is normally a side-effecting statement, not dead code. Render
        // its statement form as an IIFE prelude -- skipping the value outright
        // would silently discard kernel-body loops: a block kernel
        // `{ let mut s = 0.0; for .. { s = s + .. }; s }` inlined at a
        // method_for apply site would return the init value unchanged.
        let stmtPrelude = renderUnitStmts subst names value
        if stmtPrelude = "" then
            // Genuinely effect-free (unit literal).
            if isUnitExpr body then
                "((void)0)"
            else
                exprToCppCore subst names' body
        elif isUnitExpr body then
            // Effectful value, unit body: whole let is a void expression.
            let bodyStmts = renderUnitStmts subst names' body
            let stmts = [stmtPrelude; bodyStmts] |> List.filter (fun s -> s <> "") |> String.concat " "
            $$"""([&]() { {{stmts}} }())"""
        else
            $$"""([&]() { {{stmtPrelude}} return {{(exprToCppCore subst names' body)}}; }())"""
    else
        // The lift pass produces IRLet bindings whose value can be
        // an inline form (mask/sort/intersect/union). These can't be
        // rendered as a single C++ expression -- they need a multi-
        // statement materialization sequence. Detect that case and emit
        // an IIFE with the materialization as its prelude. The variable
        // `__v<id>` and `__v<id>_extents` come into scope for the body.
        //
        // For all other values (scalars, function calls, IRApplyCombinator
        // results, etc.), the existing "auto __v = ..." form is correct.
        // Lazy: `value` here is ANY let-bound value, not a known inline form, so
        // the element type must not be asked for until an arm needs it (see
        // materializeInlineForm's note).
        let inlineElemTypeStr (form: IRExpr) =
            lazy (inferInlineElemTypeStr "IRLet inline form" form)
        match materializeInlineForm subst names ($"__v{id}") (inlineElemTypeStr value) value with
        // Expression position: the materialization is spliced into an IIFE, so
        // there is no statement scope whose exit could carry a free. Drop the
        // descriptors (unchanged pre-existing leak).
        | Some (preludeStmts, _) ->
            let bodyStr =
                if isUnitExpr body then "((void)0)"
                else exprToCppCore subst names' body
            let prelude = preludeStmts |> String.concat " "
            $$"""([&]() { {{prelude}} return {{bodyStr}}; }())"""
        | None ->
            let valStr = exprToCppCore subst names value
            match body with
            | IRLit IRLitUnit ->
                $$"""([&]() { auto __v{{id}} = {{valStr}}; }())"""
            | b when isUnitExpr b ->
                // Unit-typed but effectful tail (assignment / for-in as the
                // block's last statement): emit its statement form rather
                // than a `return` of a statement expression.
                $$"""([&]() { auto __v{{id}} = {{valStr}}; {{(renderUnitStmts subst names' b)}} }())"""
            | _ ->
                let bodyStr = exprToCppCore subst names' body
                $$"""([&]() { auto __v{{id}} = {{valStr}}; return {{bodyStr}}; }())"""


/// Render a unit-typed side-effecting expression -- a STATEMENT that
/// lowerTypedBlock sequenced through a dummy let (assignment, for-in loop,
/// nested statement block) -- as flat C++ statement text for splicing into an
/// expression-context IIFE. Returns "" for no runtime effect (unit literal).
/// The expression-context sibling of genFuncBody's IRForRange/IRAssign arms;
/// lives in the exprToCppCore let-rec group since statements re-enter
/// expression rendering for their operands (loop bounds, RHS, indices).
and renderUnitStmts (subst: SubstMap) (names: Map<IRId, string>) (expr: IRExpr) : string =
    match expr with
    | IRLit IRLitUnit -> ""
    | IRAssign _ ->
        $"{(exprToCppCore subst names expr)};"
    | IRConstraintCheck (cond, blCode, message, span) ->
        $"if (!({(exprToCppCore subst names cond)})) {{ blade_rt::panic(\"{blCode}\", \"{message}\", {(panicSpanArgs span)}); }}"
    | IRBreakIf cond ->
        // Inside an expression-context loop render the break still binds to
        // the innermost emitted `for` (renderUnitStmts' own IRForRange arm),
        // which is the rec-array recursion loop that synthesized it. No
        // alloc-scope frees here: this flat-text path never pushes one.
        $"if ({(exprToCppCore subst names cond)}) {{ break; }}"
    | IRForRange (vid, lo, hi, body) ->
        // Same loop-var naming (__k<id>) and int64_t convention as
        // genForRangeBinding / EmitCpp.forLoopFrom, so inlined kernel
        // loops read like their module-level counterparts. int64_t, not
        // size_t: the loop var is the user's Int64 for-in variable, and an
        // unsigned binding wraps negative intermediates in body arithmetic.
        let varName = $"__k{vid}"
        let names' = Map.add vid varName names
        let loStr = exprToCppCore subst names lo
        let hiStr = exprToCppCore subst names hi
        let bodyStmts = renderUnitStmts subst names' body
        $$"""for (int64_t {{varName}} = {{loStr}}; {{varName}} < {{hiStr}}; {{varName}}++) { {{bodyStmts}} }"""
    | IRLet (letId, value, body) ->
        // Statement-position let chain (a nested block): declare non-unit
        // values, splice unit statements, continue down the chain. Inline
        // forms (mask/sort/...) get their multi-statement materialization,
        // mirroring renderLetExpr's expression-position handling.
        let names' = Map.add letId ($"__v{letId}") names
        let valueStmt =
            if isUnitExpr value then
                renderUnitStmts subst names value
            else
                // Lazy for the same reason as renderLetExpr's site above: this
                // arm sees every statement-position let value, inline form or not.
                match materializeInlineForm subst names ($"__v{letId}") (lazy (inferInlineElemTypeStr "statement-position let" value)) value with
                // Inline statement TEXT for an enclosing IIFE -- same
                // no-scope-exit situation as renderLetExpr above; drop.
                | Some (prelude, _) -> prelude |> String.concat " "
                | None -> $"auto __v{letId} = {(exprToCppCore subst names value)};"
        [valueStmt; renderUnitStmts subst names' body]
        |> List.filter (fun s -> s <> "")
        |> String.concat " "
    | IRSequence elems ->
        elems
        |> List.map (renderUnitStmts subst names)
        |> List.filter (fun s -> s <> "")
        |> String.concat " "
    | other ->
        // Not statically unit: evaluate for side effects, discard the value.
        // Also the safety net for unhandled statement forms -- a visible
        // C++ expression beats a silent drop.
        $"(void)({(exprToCppCore subst names other)});"


and renderExtentExpr (subst: SubstMap) (names: Map<IRId, string>) arr dim : string =
    // Statically resolved when the index type's extent expression is a
    // literal-arithmetic value (Idx<5>, Idx<n+1> with n compile-time, etc.)
    // -- emit as a compile-time literal eligible for use in static contexts.
    // Falls back to a runtime read from <name>_extents[dim] for genuinely
    // dynamic extents (mask, group_by groups, sort outputs derived from
    // those, etc.).
    match inferExprType arr with
    | ArrayElem at when dim < at.IndexTypes.Length ->
        match tryEvalIntIR at.IndexTypes.[dim].Extent with
        | Some n -> $"{n}L"
        | None ->
            let arrName = exprToCppCore subst names arr
            // A rank-1 ragged/dep-idx operand is a RaggedRow<T> (per
            // cppArrayTypeStr), which carries its length inline as `.len`,
            // not via a pointer-to-extents like Array<T,1>. Its only axis is
            // dim 0. Every other operand (Array, higher-rank ragged) uses the
            // materialized `.extents[dim]`.
            let isRaggedRow = isRaggedRowType at
            // A rank-1 all-dims compound (the filtered-set case,
            // compound(A, mask(A, p))) has no .extents member; its sole
            // axis's runtime extent is the compact index's cardinality.
            // (Multi-rank compound extents are rejected at typecheck,
            // same ill-posedness rule as ragged slots.)
            let isRank1Compound = (isCompoundArrayType at || isSparseArrayType at) && at.IndexTypes.Length = 1
            if isRaggedRow && dim = 0 then
                $"(int64_t)({arrName}.len)"
            elif isRank1Compound && dim = 0 then
                $"(int64_t)({arrName}.idx->cardinality)"
            else
                $"(int64_t)({arrName}.extents[{dim}])"
    | _ ->
        // `extents(range<T>)`: a virtual range has no materialized C++ object
        // to read `.extents[]` off, but its slot extents are part of the IR --
        // statically evaluable ones (Idx<11>, Idx<n_seg + 1> with n_seg
        // `let static`) emit as literals. A genuinely runtime slot extent has
        // no object either, so it stays a deliberate refusal.
        match arr with
        | IRRange (idxTys, _) when dim < idxTys.Length ->
            (match tryEvalIntIR idxTys.[dim].Extent with
             | Some n -> $"{n}L"
             | None -> exprError "extents(range<...>): the range's extent is not statically evaluable, and a virtual range has no runtime object to read it from -- take extents of a materialized array instead")
        | _ ->
            // Should be unreachable -- typecheck rejects non-arrays. Surface a
            // visible marker rather than emit garbage if the IR is malformed.
            "/* extents: argument is not an array (typechecker bug) */"


and genApplyCombinatorExpr (subst: SubstMap) (names: Map<IRId, string>) (info: ApplyInfo) : string =
    let arrayNames = 
        info.Arrays |> List.mapi (fun i arr ->
            match arr with
            | IRVar (id, _) -> Map.tryFind id names |> Option.defaultValue ($"arr{i}")
            | IRParam (name, _, _) -> name
            | _ -> $"arr{i}")
    
    // Kernel resolution is unified via `resolveCallable`; the wrapper carries
    // the kernel's own signature directly (sections are honestly typed via
    // `lowerTypedSection` + `inferReduce` kernel-param unify).
    //
    // IRReynolds-wrapped kernels are peeled -- at this inline-form emission
    // site the Reynolds symmetrization is informational and doesn't change
    // what the kernel computes per call; the symmetric accumulation is the
    // iteration structure, not the kernel itself. (See audit item 4 for the
    // open question on whether this peel is actually correctness-preserving.)
    //
    // This inline expression-position path is reached only for an
    // apply-combinator nested inside a kernel body: top-level and
    // function-body applies route through the statement-form
    // genApplyCombinator (see genFuncBody's IRCompute(IRApplyCombinator)
    // arm and genBinding). A 2-array Cartesian sum-reduce here would be a
    // SILENT MISCOMPILE for a co-iteration (zip) kernel body such as `ra * rb`
    // between two row params, producing (sum ra)(sum rb) instead of the
    // elementwise row. No correct program reaches here (verified: no
    // corpus/examples kernel has an array-valued elementwise body), so reject
    // loudly rather than emit wrong code.
    exprError ($"array-valued elementwise kernel body not supported inside a kernel ({arrayNames.Length}-array inline combinator); reduce the row to a scalar with prodsum or reduce, or compute the elementwise product at top level")

/// Convert IRExpr to C++ with an additional variable binding
and exprToCppWithVarCore (subst: SubstMap) (names: Map<IRId, string>) (varId: IRId) (varName: string) (expr: IRExpr) : string =
    let names' = Map.add varId varName names
    exprToCppCore subst names' expr

/// Convenience wrapper: render with no contains-substitution. This is the
/// API every existing caller uses; the substitution-aware path goes through
/// exprToCppWithSubst (defined outside the let-rec group).
///
/// Wrappers are inside the recursion group because sibling helpers
/// (`genApplyCombinatorExpr`, `materializeInlineForm`) reference these
/// names; defining them as plain `let` after the group would push them
/// out of scope at those call sites.
and exprToCpp (names: Map<IRId, string>) (expr: IRExpr) : string =
    exprToCppCore emptySubst names expr

and exprToCppWithVar (names: Map<IRId, string>) (varId: IRId) (varName: string) (expr: IRExpr) : string =
    exprToCppWithVarCore emptySubst names varId varName expr

/// Generate the C++ statements that materialize an inline form (IRMask,
/// IRIntersect, IRUnion, IRSort, ...) into `varName` and `varName + "_extents"`.
/// Returns statements WITHOUT leading indentation; callers add their own
/// prefix. `elemTypeStr` is the caller-resolved element type: genBinding uses
/// inferElemTypeStrict (#error on unresolvable), genFuncBody/exprToCpp IIFE use
/// silent fallback -- pulled out here to stay format-neutral and let each
/// caller surface errors for its own context. Mutually recursive with
/// exprToCpp to render nested predicate/key bodies; None for forms outside
/// this set. Each builder returns its lines PLUS `MaterializedAlloc`
/// descriptors for whatever IT allocated; statement-position consumers hand
/// those to `registerMaterializedAllocs` to free at scope exit, expression/IIFE
/// consumers drop them and leak.
///
/// `elemTypeStr` is a `Lazy` and NOT an eager string, because the two
/// expression-position callers (renderLetExpr, renderUnitStmts' IRLet arm)
/// cannot pre-filter: they hand over WHATEVER a let is bound to and use the
/// `None` result to mean "render this the ordinary `auto __v = ...` way".
/// Resolving an element type for those values eagerly asked the question of
/// things that legitimately have no single element type -- a tuple binding
/// (`let t = B, C`) most visibly -- and `inferInlineElemTypeStr` answered by
/// collecting a "likely a typechecker or IR bug" warning for a string this
/// function then discarded on the `_ -> None` arm. Deferring it here ties the
/// question to the arms that actually consume the answer, so the arm list
/// below is the single definition of which forms need one; a caller cannot
/// drift out of step with it.
and materializeInlineForm (subst: SubstMap) (names: Map<IRId, string>) (varName: string) (elemTypeStr: Lazy<string>) (form: IRExpr) : (string list * MaterializedAlloc list) option =
    match form with
    | IRMask (arrExpr, predExpr) ->
        materializeMaskForm subst names varName elemTypeStr.Value arrExpr predExpr
    | IRIntersect (aExpr, bExpr) ->
        materializeIntersectForm subst names varName elemTypeStr.Value aExpr bExpr
    | IRUnion (aExpr, bExpr) ->
        materializeUnionForm subst names varName elemTypeStr.Value aExpr bExpr
    | IRUnique aExpr ->
        materializeUniqueForm subst names varName elemTypeStr.Value aExpr
    | IRSort (arrExpr, keyExpr) ->
        materializeSortForm subst names varName elemTypeStr.Value arrExpr keyExpr
    | IRTranspose (arrExpr, d1, d2) ->
        materializeTransposeForm subst names varName elemTypeStr.Value arrExpr d1 d2
    | IRStack arrs ->
        materializeStackForm subst names varName elemTypeStr.Value arrs
    | IRJoin (arrs, dim) ->
        materializeJoinForm subst names varName elemTypeStr.Value arrs dim
    | IRDecompact (arrExpr, dimArg) ->
        materializeDecompactForm subst names varName elemTypeStr.Value arrExpr dimArg
    | IRArrayNegate arrExpr | IRArrayConjugate arrExpr ->
        materializeNegateConjugateForm subst names varName elemTypeStr.Value form arrExpr
    | IRGram (lExpr, rExpr, sameArray) ->
        materializeGramForm subst names varName elemTypeStr.Value lExpr rExpr sameArray
    | IRGramApply (lExpr, rExpr, xExpr) ->
        materializeGramApplyForm subst names varName elemTypeStr.Value lExpr rExpr xExpr
    | IRMatmul (lExpr, rExpr) ->
        materializeMatmulForm subst names varName elemTypeStr.Value lExpr rExpr
    // `elemTypeStr` is deliberately NOT forwarded: eigh produces TWO pools whose
    // element types can differ (complex Q, real LAM), so a single caller-supplied
    // element string cannot describe the result. The form derives both from the
    // OPERAND's type, which is the only place the pair is jointly determined.
    // Under the `Lazy` above this arm therefore never forces one either, which
    // is what inferInlineElemTypeStr's IREigh arm was working around.
    | IREigh operand ->
        materializeEighForm subst names varName operand
    | IRSolve (mExpr, rExpr) ->
        materializeSolveForm subst names varName elemTypeStr.Value mExpr rExpr
    // `elemTypeStr` not forwarded for lu: a TUPLE value (Float64 factor,
    // Int64 pivots), both derived from the operand, as for eigh.
    | IRLu mExpr ->
        materializeLuForm subst names varName mExpr
    | IRLuSolve (lExpr, pExpr, rExpr, transposed) ->
        materializeLuSolveForm subst names varName elemTypeStr.Value lExpr pExpr rExpr transposed
    // `elemTypeStr` not forwarded: a range's element is always Int64, and the
    // caller-side inference has no arm for a bare range (same Lazy discipline
    // as IREigh above -- this arm must not force it).
    | IRRange (ixs, offset) ->
        materializeRangeForm subst names varName ixs offset
    | _ -> None


and materializeRangeForm (subst: SubstMap) (names: Map<IRId, string>) (varName: string) (ixs: IRIndexType list) (offset: IRExpr option) : (string list * MaterializedAlloc list) option =
    // A BARE range in value position (`let xs = 0..n` / `let r = range<I>`,
    // `reduce(0..n, (+))` via the lift pass, either behind `|> compute`):
    // materialize the iota it denotes, x[i] = offset + i. Inside a combinator
    // a range never becomes a value -- the nest peels it as induction values,
    // which is the whole point of a virtual array; this arm is only for the
    // positions that consume a materialized array by name.
    //
    // Only the single-slot plain dense form has a standalone value meaning:
    // compound/sparse/halo slots enumerate coordinate SETS (their "element"
    // is not a storable position), and a multi-slot range only means anything
    // to a loop nest -- those answer None and keep their refusal.
    match ixs with
    | [ ix ] when ix.IxKind = IxKPlain && ix.Rank = 1
                  && (match ix.Tag with
                      | Some t -> not (t.StartsWith haloWinTagPrefix)
                      | None -> true) ->
        let boundDim =
            match tryEvalIntIR ix.Extent with
            | Some n -> (string n, true)
            | None -> ($"(size_t)({exprToCppCore subst names ix.Extent})", false)
        let (extentsDecl, ownedExtents) =
            emitExtentsTable "" ($"{varName}_extents") 1 [boundDim]
        let bound = fst boundDim
        let offStr =
            match offset with
            | Some o -> exprToCppCore subst names o
            | None -> "0"
        Some (
            extentsDecl @ [
                $$"""Array<int64_t, 1> {{varName}} = { new int64_t[{{bound}}], {{varName}}_extents };"""
                $"for (size_t __ri = 0; __ri < {bound}; __ri++) {varName}[__ri] = (int64_t)__ri + (int64_t)({offStr});"
            ],
            [MatRawData (varName, ownedExtents)]
        )
    | _ -> None


and materializeMaskForm (subst: SubstMap) (names: Map<IRId, string>) (varName: string) (elemTypeStr: string) (arrExpr: IRExpr) (predExpr: IRExpr) : (string list * MaterializedAlloc list) option =
    // mask(A, pred) -> the Bool PRESENCE array over A's own index space,
    // m[i] = pred(A[i]). One pass, no value copying: compaction belongs
    // to compound(A, m); iteration to range<CompoundIdx<m>>. A contains(B, x)
    // inside the predicate renders as a linear scan (see the note on the
    // predicate arm below).
    let arrName = exprToCppCore subst names arrExpr
    let maskRank =
        match inferExprType arrExpr with
        | ArrayElem a -> a.IndexTypes.Length
        | _ -> 1
    // A RaggedRow-typed source (mask over a peeled row param) carries its
    // length inline as .len; everything else reads .extents[0].
    let srcBoundDim =
        match inferExprType arrExpr with
        | ArrayElem a when isRaggedRowType a -> ($"{arrName}.len", false)
        | ArrayElem a -> extentDimOfArray a arrName 0
        | _ -> ($"{arrName}.extents[0]", false)
    let srcBound = fst srcBoundDim
    // Companion extents table under the shared rule (emitExtentsTable): heap
    // when the bound is a runtime read, static constexpr when the source's
    // index record pinned it. NOT a frame-local `size_t[1]` -- this mask can be
    // returned out of the function body that built it.
    let (extentsDecl, ownedExtents) =
        emitExtentsTable "" ($"{varName}_extents") 1 [srcBoundDim]
    if maskRank <> 1 then
        Some ([refusalErrorLine "" ($"Blade codegen: mask over a rank-{maskRank} array is not yet supported (rank-1 only for now; rank-k masks land with the compound composition round)")], [])
    else
    match resolveCallable predExpr with
    | Some callable when callable.Params.Length = 1 ->
        // Emit the per-element predicate call; any contains inside the
        // predicate runs as a linear scan (genCallableWrapper calls the
        // predicate by NAME, so a hoisted set would be unqueried).
        let (wrapperCode, wname) = genCallableWrapper names varName callable
        let predParamName = $"__{varName}_x"
        // Source element type (elemTypeStr is the RESULT type, i.e. bool).
        let srcElemStr =
            match inferExprType arrExpr with
            | ArrayElem a -> elemTypeToCpp a.ElemType
            | _ -> "double"
        Some (
            wrapperCode @ extentsDecl @ [
                $$"""Array<bool, 1> {{varName}} = { new bool[{{srcBound}}], {{varName}}_extents };"""
                $$"""for (size_t __mi = 0; __mi < {{srcBound}}; __mi++) {"""
                $"    {srcElemStr} {predParamName} = {arrName}[__mi];"
                $"    {varName}[__mi] = {wname}({predParamName});"
                "}"
            ],
            // Raw `new bool[n]` backing plus whatever `<varName>_extents` turned
            // out to own. A downstream compound() copies the bits into a
            // std::vector<bool> at construction (genCompoundIndexFromMask), so
            // nothing outlives this scope by pointing INTO the mask.
            [MatRawData (varName, ownedExtents)]
        )
    | _ ->
        // Degenerate (unresolved predicate): all-true mask; #error would be
        // kinder but this mirrors the prior fallback's shape.
        Some (extentsDecl @ [
            $$"""Array<bool, 1> {{varName}} = { new bool[{{srcBound}}], {{varName}}_extents };"""
            $"for (size_t __mi = 0; __mi < {srcBound}; __mi++) {varName}[__mi] = true;"
        ], [MatRawData (varName, ownedExtents)])



and materializeIntersectForm (subst: SubstMap) (names: Map<IRId, string>) (varName: string) (elemTypeStr: string) (aExpr: IRExpr) (bExpr: IRExpr) : (string list * MaterializedAlloc list) option =
    // SQL INTERSECT: unique values appearing in BOTH arrays, output in
    // first-occurrence order from A. Two-pass with set reuse, mirroring
    // unique() -- first pass counts unique A-elements that are also in
    // B, second pass emits them in order.
    //
    // The `__seen.insert(x).second` idiom is a one-shot "is-first?"
    // check: returns true iff x wasn't previously in the set. Used in
    // both passes so each unique A-element is counted exactly once
    // (regardless of how often it repeats in A).
    let aName = exprToCppCore subst names aExpr
    let bName = exprToCppCore subst names bExpr
    // The cardinality is data-dependent, so this is always the heap arm of the
    // shared companion-extents rule -- which is what lets the result be
    // returned out of the function body that computed it.
    let (extentsDecl, ownedExtents) =
        emitExtentsTable "" ($"{varName}_extents") 1 [($"{varName}__count", false)]
    Some ([
        $"std::unordered_set<{elemTypeStr}> {varName}__b_set;"
        $"for (size_t __si = 0; __si < {bName}.extents[0]; __si++) {varName}__b_set.insert({bName}[__si]);"
        $"std::unordered_set<{elemTypeStr}> {varName}__seen;"
        $"size_t {varName}__count = 0;"
        $$"""for (size_t __si = 0; __si < {{aName}}.extents[0]; __si++) {"""
        $"    {elemTypeStr} __x = {aName}[__si];"
        $"    if ({varName}__b_set.count(__x) && {varName}__seen.insert(__x).second) {varName}__count++;"
        "}"
    ] @ extentsDecl @ [
        $$"""Array<{{elemTypeStr}}, 1> {{varName}} = { new {{elemTypeStr}}[{{varName}}__count], {{varName}}_extents };"""
        $"{varName}__seen.clear();"
        $"size_t {varName}__fill = 0;"
        $$"""for (size_t __si = 0; __si < {{aName}}.extents[0]; __si++) {"""
        $"    {elemTypeStr} __x = {aName}[__si];"
        $"    if ({varName}__b_set.count(__x) && {varName}__seen.insert(__x).second) {varName}[{varName}__fill++] = __x;"
        "}"
    ], [MatRawData (varName, ownedExtents)])


and materializeUnionForm (subst: SubstMap) (names: Map<IRId, string>) (varName: string) (elemTypeStr: string) (aExpr: IRExpr) (bExpr: IRExpr) : (string list * MaterializedAlloc list) option =
    // SQL UNION: unique values appearing in EITHER array, output in
    // first-occurrence order across the concatenation A ++ B. Two-pass
    // with set reuse. Each pass walks A then B; the shared seen set
    // ensures A's elements appear before B's, and within each, only
    // first occurrences survive.
    let aName = exprToCppCore subst names aExpr
    let bName = exprToCppCore subst names bExpr
    // Data-dependent cardinality: always the heap arm (see the intersect form).
    let (extentsDecl, ownedExtents) =
        emitExtentsTable "" ($"{varName}_extents") 1 [($"{varName}__count", false)]
    Some ([
        $"std::unordered_set<{elemTypeStr}> {varName}__seen;"
        $"size_t {varName}__count = 0;"
        $$"""for (size_t __si = 0; __si < {{aName}}.extents[0]; __si++) {"""
        $"    if ({varName}__seen.insert({aName}[__si]).second) {varName}__count++;"
        "}"
        $$"""for (size_t __si = 0; __si < {{bName}}.extents[0]; __si++) {"""
        $"    if ({varName}__seen.insert({bName}[__si]).second) {varName}__count++;"
        "}"
    ] @ extentsDecl @ [
        $$"""Array<{{elemTypeStr}}, 1> {{varName}} = { new {{elemTypeStr}}[{{varName}}__count], {{varName}}_extents };"""
        $"{varName}__seen.clear();"
        $"size_t {varName}__fill = 0;"
        $$"""for (size_t __si = 0; __si < {{aName}}.extents[0]; __si++) {"""
        $"    if ({varName}__seen.insert({aName}[__si]).second) {varName}[{varName}__fill++] = {aName}[__si];"
        "}"
        $$"""for (size_t __si = 0; __si < {{bName}}.extents[0]; __si++) {"""
        $"    if ({varName}__seen.insert({bName}[__si]).second) {varName}[{varName}__fill++] = {bName}[__si];"
        "}"
    ], [MatRawData (varName, ownedExtents)])


and materializeUniqueForm (subst: SubstMap) (names: Map<IRId, string>) (varName: string) (elemTypeStr: string) (aExpr: IRExpr) : (string list * MaterializedAlloc list) option =
    // First pass: insert each element into an unordered_set; count
    // first-occurrences. Second pass: clear the set, rescan, emit on
    // first occurrence. Two passes keep allocation exact (no
    // intermediate vector) while preserving first-occurrence order.
    let aName = exprToCppCore subst names aExpr
    // Data-dependent cardinality: always the heap arm (see the intersect form).
    let (extentsDecl, ownedExtents) =
        emitExtentsTable "" ($"{varName}_extents") 1 [($"{varName}__count", false)]
    Some ([
        $"std::unordered_set<{elemTypeStr}> {varName}__seen;"
        $"size_t {varName}__count = 0;"
        $$"""for (size_t __ui = 0; __ui < {{aName}}.extents[0]; __ui++) {"""
        $"    if ({varName}__seen.insert({aName}[__ui]).second) {varName}__count++;"
        "}"
    ] @ extentsDecl @ [
        $$"""Array<{{elemTypeStr}}, 1> {{varName}} = { new {{elemTypeStr}}[{{varName}}__count], {{varName}}_extents };"""
        $"{varName}__seen.clear();"
        $"size_t {varName}__fill = 0;"
        $$"""for (size_t __ui = 0; __ui < {{aName}}.extents[0]; __ui++) {"""
        $"    if ({varName}__seen.insert({aName}[__ui]).second) {varName}[{varName}__fill++] = {aName}[__ui];"
        "}"
    ], [MatRawData (varName, ownedExtents)])


and materializeSortForm (subst: SubstMap) (names: Map<IRId, string>) (varName: string) (elemTypeStr: string) (arrExpr: IRExpr) (keyExpr: IRExpr) : (string list * MaterializedAlloc list) option =
    // Key-callable resolution via `resolveCallable`. The key
    // function emits as a local wrapper closure
    // (`__wrap_<id>_<varName>`) that forwards to the lifted
    // function with captures pulled by reference. The wrapper
    // takes the element value as its single arg and returns
    // the orderable key; the stable_sort's comparator invokes
    // the wrapper on each element under comparison.
    //
    // Fallback for unresolved keyExpr (shouldn't happen for
    // well-typed sort calls): emit a sort that's a no-op on
    // key (returns literal 0 -- all elements compare equal,
    // preserving input order under stable_sort).
    let arrName = exprToCppCore subst names arrExpr
    // A rank-1 compound operand (compound(A, mask(A, p)) -- the filtered
    // set) sorts its compact buffer: bound = cardinality, elements via
    // .data[i]. Sorting discards coordinate meaning by construction, so
    // the DENSE output shape is the semantically honest one. Dense
    // operands keep .extents/operator[].
    let isR1Compound =
        match inferExprType arrExpr with
        | ArrayElem at -> (isCompoundArrayType at || isSparseArrayType at) && at.IndexTypes.Length = 1
        | _ -> false
    let srcBoundDim =
        if isR1Compound then ($"{arrName}.idx->cardinality", false)
        else
            match inferExprType arrExpr with
            | ArrayElem at -> extentDimOfArray at arrName 0
            | _ -> ($"{arrName}.extents[0]", false)
    let srcBound = fst srcBoundDim
    // Shared companion-extents rule: a sorted result is a value like any other
    // and may be returned, so the table must outlive this frame.
    let (extentsDecl, ownedExtents) =
        emitExtentsTable "" ($"{varName}_extents") 1 [srcBoundDim]
    let srcAt (i: string) = if isR1Compound then $"{arrName}.data[{i}]" else $"{arrName}[{i}]"
    let (wrapperCode, keyCall) =
        match resolveCallable keyExpr with
        | Some callable when callable.Params.Length = 1 ->
            let (code, wname) = genCallableWrapper names varName callable
            (code, wname)
        | _ -> ([], "[](auto) { return 0; }")  // degenerate fallback
    Some (
        wrapperCode @ [
            $"size_t* {varName}__perm = new size_t[{srcBound}];"
            $"for (size_t __pi = 0; __pi < {srcBound}; __pi++) {varName}__perm[__pi] = __pi;"
            $$"""std::stable_sort({{varName}}__perm, {{varName}}__perm + {{srcBound}}, [&](size_t __a, size_t __b) {"""
            $"""    return {keyCall}({(srcAt "__a")}) < {keyCall}({(srcAt "__b")});"""
            "});"
        ] @ extentsDecl @ [
            $$"""Array<{{elemTypeStr}}, 1> {{varName}} = { new {{elemTypeStr}}[{{srcBound}}], {{varName}}_extents };"""
            $"""for (size_t __si = 0; __si < {srcBound}; __si++) {varName}[__si] = {(srcAt (sprintf "%s__perm[__si]" varName))};"""
        ],
        // Two raw buffers: the permutation scratch (`size_t*`, dead after the
        // gather) and the output backing behind the Array<T,1> wrapper, the
        // latter carrying the extents table it owns.
        [MatRawBuf (varName + "__perm"); MatRawData (varName, ownedExtents)]
    )


and materializeTransposeForm (subst: SubstMap) (names: Map<IRId, string>) (varName: string) (elemTypeStr: string) (arrExpr: IRExpr) (d1: int) (d2: int) : (string list * MaterializedAlloc list) option =
    // Hard transpose: allocate a fresh pool at the SWAPPED extents and copy
    // every element with axes d1/d2 exchanged. The result is an independent
    // array (new pool, new row-pointers) -- no aliasing back to the source,
    // which is why this is always correct (never a soft/view transpose).
    // General rank: an N-deep nested loop over the SOURCE extents; the
    // destination subscript list is the source loop vars with positions
    // d1 and d2 swapped. TypeCheck guarantees both axes are arity-1 SymNone,
    // so the source is rectangular and every dim is a single plain Idx.
    let arrName = exprToCppCore subst names arrExpr
    (match inferExprType arrExpr with
     | ArrayElem arrTy ->
        let rank = arrTy.IndexTypes.Length
        let extentsName = $"{varName}_extents"
        // Source loop variables, one per dimension.
        let srcVar d = $"__t{varName}_{d}"
        // Destination extents = source extents with d1/d2 swapped.
        let swapDim d = if d = d1 then d2 elif d = d2 then d1 else d
        // Shared companion-extents rule (emitExtentsTable): the transposed
        // wrapper is returnable, so the table is static-constexpr when every
        // swapped axis is pinned and heap otherwise -- never frame-local.
        let (extentDecl, ownedExtents) =
            emitExtentsTable "" extentsName rank
                [ for d in 0 .. rank - 1 -> extentDimOfArray arrTy arrName (swapDim d) ]
        let allocDecl =
            arrayAlloc { Ind = ""; Elem = elemTypeStr; Rank = rank; Name = varName
                         Symm = "nullptr"; Strict = None; Extents = extentsName }
        // Nested copy loops over the SOURCE extents. Bounds go through the
        // shared literal-or-runtime rule, so a source with statically pinned
        // axes copies under literal trip counts (the extents TABLE above is a
        // shape record consumed once by allocate<> and stays a runtime copy).
        let openLoops =
            [ for d in 0 .. rank - 1 ->
                let ind = String.replicate d "    "
                $"{ind}for (size_t {(srcVar d)} = 0; {(srcVar d)} < {(literalOrRuntimeExtentOfArray arrTy arrName d)}; {(srcVar d)}++) {{" ]
        // dst's dimension d is fed by source dimension swapDim(d).
        let srcIdx = [ for d in 0 .. rank - 1 -> $"[{(srcVar d)}]" ] |> String.concat ""
        let dstIdx = [ for d in 0 .. rank - 1 -> $"[{(srcVar (swapDim d))}]" ] |> String.concat ""
        let bodyInd = String.replicate rank "    "
        let body = [ $"{bodyInd}{varName}{dstIdx} = {arrName}{srcIdx};" ]
        let closeLoops = [ for d in rank - 1 .. -1 .. 0 -> $"""{(String.replicate d "    ")}}}""" ]
        Some (extentDecl @ [allocDecl] @ openLoops @ body @ closeLoops,
              [MatPool (varName, elemTypeStr, rank, "nullptr", None, ownedExtents)])
     | _ -> None)


and materializeStackForm (subst: SubstMap) (names: Map<IRId, string>) (varName: string) (elemTypeStr: string) (arrs: IRExpr list) : (string list * MaterializedAlloc list) option =
    // stack(A1, ..., An) (formalism 2.6): a FRESH LEADING AXIS of extent n over
    // n same-shaped arrays, so `stack(A,B,C)(k)` selects array k. Rank r -> r+1.
    //
    // Materialized as an independent dense pool plus a per-source copy nest --
    // deliberately NOT a pointer-aliasing assembly (`out[k] = Ak`, which is what
    // the sibling IRSequence emitter does). `let` bindings are assignable in
    // Blade, so an aliased stack would see later writes to any source; copying
    // makes the combinator a value, matching transpose/decompact/gram.
    //
    // Extents come from the FIRST source at runtime (TypeCheck has already
    // proven every operand has the same rank/extents/element type, and that
    // every slot is a plain dense Idx).
    match arrs with
    | [] -> None
    | first :: _ ->
        let srcNames = arrs |> List.map (exprToCppCore subst names)
        let firstName = List.head srcNames
        (match inferExprType first with
         | ArrayElem at ->
            let srcRank = at.IndexTypes.Length
            let outRank = srcRank + 1
            let extentsName = $"{varName}_extents"
            // Shared companion-extents rule: the fresh leading axis is always a
            // literal (the operand COUNT), so the table goes static-constexpr
            // exactly when the sources' own axes are pinned too.
            let (extentDecl, ownedExtents) =
                emitExtentsTable "" extentsName outRank
                    ((string arrs.Length, true)
                     :: [ for d in 0 .. srcRank - 1 -> extentDimOfArray at firstName d ])
            let allocDecl =
                arrayAlloc { Ind = ""; Elem = elemTypeStr; Rank = outRank; Name = varName
                             Symm = "nullptr"; Strict = None; Extents = extentsName }
            // One copy nest per source. The loop variables are declared in each
            // `for` init, so sibling nests at the same level reuse the names
            // without colliding.
            let loopVar d = $"__sk{varName}_{d}"
            // Each nest reads its OWN source's index records, not the first
            // source's: TypeCheck has proven the operands share extents at
            // runtime, but not that they were all pinned to literals by shape
            // monomorphization, so a source that knows its own trip count
            // should bake it whatever its siblings managed.
            let srcTypes = arrs |> List.map inferExprType
            let copyNest (k: int) (srcName: string) =
                let srcBoundAt d =
                    match List.item k srcTypes with
                    | ArrayElem st -> literalOrRuntimeExtentOfArray st srcName d
                    | _ -> $"{srcName}.extents[{d}]"
                let opens =
                    [ for d in 0 .. srcRank - 1 ->
                        let ind = String.replicate d "    "
                        $"{ind}for (size_t {(loopVar d)} = 0; {(loopVar d)} < {(srcBoundAt d)}; {(loopVar d)}++) {{" ]
                let srcIdx = [ for d in 0 .. srcRank - 1 -> $"[{(loopVar d)}]" ] |> String.concat ""
                let bodyInd = String.replicate srcRank "    "
                let body = [ $"{bodyInd}{varName}[{k}]{srcIdx} = {srcName}{srcIdx};" ]
                let closes = [ for d in srcRank - 1 .. -1 .. 0 -> $"""{(String.replicate d "    ")}}}""" ]
                opens @ body @ closes
            Some (extentDecl @ [allocDecl] @ (srcNames |> List.mapi copyNest |> List.concat),
                  [MatPool (varName, elemTypeStr, outRank, "nullptr", None, ownedExtents)])
         | _ -> None)


and materializeJoinForm (subst: SubstMap) (names: Map<IRId, string>) (varName: string) (elemTypeStr: string) (arrs: IRExpr list) (dim: int) : (string list * MaterializedAlloc list) option =
    // join(A, B, ..., d) (formalism 2.6): concatenate along dimension d. Rank is
    // preserved; extents[d] is the sum of the sources' extents[d] and every
    // other axis agrees. `split(A, d, i)` = two `subset`s, so `join` is the
    // inverse half of the split-join round-trip.
    //
    // Emitted as one dense allocation plus a per-source copy nest offset by a
    // running cursor along d -- the same fresh-pool discipline as stack.
    match arrs with
    | [] -> None
    | first :: _ ->
        let srcNames = arrs |> List.map (exprToCppCore subst names)
        let firstName = List.head srcNames
        (match inferExprType first with
         | ArrayElem at ->
            let rank = at.IndexTypes.Length
            let extentsName = $"{varName}_extents"
            let joinedExtent = srcNames |> List.map (fun s -> $"{s}.extents[{dim}]") |> String.concat " + "
            // Shared companion-extents rule. The CONCATENATED axis is a sum of
            // runtime reads, so a join always lands on the heap arm -- which is
            // the arm that survives being returned.
            let (extentDecl, ownedExtents) =
                emitExtentsTable "" extentsName rank
                    [ for d in 0 .. rank - 1 ->
                        if d = dim then (joinedExtent, false)
                        else extentDimOfArray at firstName d ]
            let allocDecl =
                arrayAlloc { Ind = ""; Elem = elemTypeStr; Rank = rank; Name = varName
                             Symm = "nullptr"; Strict = None; Extents = extentsName }
            let offName = $"{varName}_joff"
            let loopVar d = $"__jn{varName}_{d}"
            // Per-source records, same reasoning as stack: the concatenated
            // axis differs between operands by construction, so the first
            // source's extents are not the others' even in principle.
            let srcTypes = arrs |> List.map inferExprType
            let copyNest (k: int) (srcName: string) =
                let srcBoundAt d =
                    match List.item k srcTypes with
                    | ArrayElem st -> literalOrRuntimeExtentOfArray st srcName d
                    | _ -> $"{srcName}.extents[{d}]"
                let opens =
                    [ for d in 0 .. rank - 1 ->
                        let ind = String.replicate d "    "
                        $"{ind}for (size_t {(loopVar d)} = 0; {(loopVar d)} < {(srcBoundAt d)}; {(loopVar d)}++) {{" ]
                let srcIdx = [ for d in 0 .. rank - 1 -> $"[{(loopVar d)}]" ] |> String.concat ""
                let dstIdx =
                    [ for d in 0 .. rank - 1 ->
                        if d = dim then $"[{(loopVar d)} + {offName}]"
                        else $"[{(loopVar d)}]" ] |> String.concat ""
                let bodyInd = String.replicate rank "    "
                let body = [ $"{bodyInd}{varName}{dstIdx} = {srcName}{srcIdx};" ]
                let closes = [ for d in rank - 1 .. -1 .. 0 -> $"""{(String.replicate d "    ")}}}""" ]
                opens @ body @ closes @ [ $"{offName} += {srcName}.extents[{dim}];" ]
            Some (extentDecl @ [allocDecl; $"size_t {offName} = 0;"]
                  @ (srcNames |> List.mapi copyNest |> List.concat),
                  [MatPool (varName, elemTypeStr, rank, "nullptr", None, ownedExtents)])
         | _ -> None)


and materializeDecompactForm (subst: SubstMap) (names: Map<IRId, string>) (varName: string) (elemTypeStr: string) (arrExpr: IRExpr) (dimArg: int) : (string list * MaterializedAlloc list) option =
    // Decompaction = binary group FISSION. decompact(A, d) isolates the
    // logical dimension d of a compact group as a free Idx, cutting on BOTH
    // sides: SymIdx<r,n> -> SymIdx<dPos,n> -> Idx<n> -> SymIdx<r-dPos-1,n>.
    // Edges degenerate to a single cut. Storage is value-equivalent to the
    // source but strictly larger (fission breaks the inter-axis dependency,
    // so each sub-group ranges over the full [0,n) again) -- the cost paid to
    // make the freed axis densely indexable / transposable.
    //
    // TWO emitted shapes:
    //   (1) General SYMMETRIC fission, any rank, any d (sole compact slot):
    //       GATHER into a fission-shaped output allocated with a per-group
    //       SYMM mask {left-run | freed-singleton | right-run}. Each output
    //       cell is written exactly once: enumerate left-group canonical
    //       coords (left-justified), freed dense axis, right-group canonical
    //       coords; assemble the logical r-tuple; sort; read the source at
    //       its left-justified canonical address. (Validated rank 2-5, all
    //       cut positions, against the runtime allocator.)
    //   (2) ANTISYMMETRIC rank-2 (fully dissolves to dense nxn): a
    //       two-image scatter with sign on the mirror and zeroed diagonal.
    //   (3) ANTISYMMETRIC rank>=3 (general, any cut): per-group-strict
    //       (allocate_strict) fission into the chain Antisym<aLen> -> Idx ->
    //       Antisym<bLen>, with the full-tuple sign baked at scatter and each
    //       residual group's own antisymmetry applied lazily on read. Handles
    //       boundary cuts (one residual group), one-sided interior cuts
    //       (rank 4: one group + a degenerate plain residual), two-sided
    //       interior cuts (rank 5: two groups), and the rank-3 interior case
    //       (both residuals degenerate -> fully dense).
    //   (4) OrbIdx (depth >= 2) FULL decompaction -- see the wreath arm below.
    let arrName = exprToCppCore subst names arrExpr
    (match inferExprType arrExpr with
     // ----- OrbIdx (iterated-wreath) FULL decompaction -----
     //
     // docs/plan-orbidx-decompaction.md section 2 + section 4, endpoint only: the whole pool
     // to the dense rank-prod(ri) tensor. `dimArg` is the number of LEVELS TO
     // KEEP (section 4.3); TypeCheck has already refused every value but 0.
     //
     // DENSE-SEQUENTIAL by choice, not a pool-sequential scatter (section 4.2): a
     // scatter needs each pool cell's ORBIT (every group element applied to
     // the canonical tuple, with its character), and no verified emitter for
     // that exists -- orb_visit hands out canonical tuples only. orb_read IS
     // the verified section 2 read, so the dense walk is one call per cell, matching
     // the interpreter's `decompactArray` algorithm exactly (so the
     // differential harness compares implementations, not designs).
     //
     // No in-place variant: section 5 -- pack and dense share no layout.
     | ArrayElem arrTy when (match arrTy.IndexTypes with
                             | [ ix ] -> ix.Symmetry = SymWreath
                             | _ -> false) ->
        let ix = List.head arrTy.IndexTypes
        let axes = max 1 ix.Rank
        let levelArgs = orbLevelArgs (orbitLevelsOf ix)
        let nStr = exprToCppCore subst names (orbitBaseExtent ix)
        let extentsName = $"{varName}_extents"
        let lv k = $"__odc{varName}_{k}"
        let coordBuf = $"__odc{varName}_c"
        // Shared companion-extents rule. The orbit base extent is rendered
        // through a cast expression rather than a bare literal, so this is the
        // heap arm unconditionally -- correct, and returnable.
        let (extentDecl, ownedExtents) =
            emitExtentsTable "" extentsName axes
                [ for _ in 0 .. axes - 1 -> ($"(size_t)({nStr})", false) ]
        let allocDecl =
            arrayAlloc { Ind = ""; Elem = elemTypeStr; Rank = axes; Name = varName
                         Symm = "nullptr"; Strict = None; Extents = extentsName }
        let opens =
            [ for k in 0 .. axes - 1 ->
                $"""{(String.replicate k "    ")}for (size_t {(lv k)} = 0; {(lv k)} < (size_t)({nStr}); {(lv k)}++) {{""" ]
        let bodyInd = String.replicate axes "    "
        let dstSubs = [ for k in 0 .. axes - 1 -> $"[{(lv k)}]" ] |> String.concat ""
        let coordInit = [ for k in 0 .. axes - 1 -> $"(int){(lv k)}" ] |> String.concat ", "
        let body =
            [ $$"""{{bodyInd}}int {{coordBuf}}[{{axes}}] = { {{coordInit}} };"""
              $"{bodyInd}{varName}{dstSubs} = orbit_wreath_utilities::orb_read<{elemTypeStr}, {levelArgs}>({arrName}, {coordBuf}, (int)({nStr}));" ]
        let closes = [ for k in axes - 1 .. -1 .. 0 -> $"""{(String.replicate k "    ")}}}""" ]
        Some (extentDecl @ [ allocDecl ] @ opens @ body @ closes,
              [ MatPool (varName, elemTypeStr, axes, "nullptr", None, ownedExtents) ])
     | ArrayElem arrTy ->
        // The compact group being decompacted is the LAST index slot
        // (TypeCheck enforces: any preceding slots are plain free Idx
        // singletons). Read the group's arity r and symmetry from that last
        // slot; the leading free slots become an outer loop product that
        // wraps the fission scatter. `leadingN` = number of leading free
        // dims; their extents are emitted before the group's freed/expanded
        // axes, and their indices map identically source->dest.
        let leadingN = max 0 (arrTy.IndexTypes.Length - 1)
        let (r, sym) =
            match List.tryLast arrTy.IndexTypes with
            | Some ix -> (max 1 ix.Rank, ix.Symmetry)
            | None -> (0, SymNone)
        // Leading free loop variables and the per-dimension subscript they
        // contribute (prefixed to both the output and source addresses).
        let leadVar j = $"__dc{varName}_S{j}"
        let leadSubs = [ for j in 0 .. leadingN - 1 -> $"[{(leadVar j)}]" ] |> String.concat ""
        let extentsName = $"{varName}_extents"
        // `n` is the shared extent of EVERY axis of the fission, and this one
        // binding renders every bound the emitter produces -- the gather nests,
        // the scatter nests, and the output extents table. Reading it through
        // the shared rule is what lets a compact source bake: its sole record is
        // the group (`AntiIdx<r, n>`, arity r), whose extent is dense axis 0's,
        // and the prefix-sum walk in `literalOrRuntimeExtentOfArray` is what
        // makes that legible. Decompaction is precisely the case where the
        // literal pays: the freed axes re-range over the full [0, n) and the
        // inner ones are short.
        let nDim = extentDimOfArray arrTy arrName 0
        let nExpr = fst nDim
        (match sym with
         | SymSymmetric when r >= 2 ->
            // ----- General symmetric fission (gather) -----
            // The targeted group is the LAST slot, preceded by `leadingN`
            // free singleton dims (global indices 0..leadingN-1). The cut's
            // position WITHIN the group is therefore the global dim minus
            // the leading count -- NOT the global dim itself. (For the sole-
            // slot case leadingN=0 so they coincide, which is why this only
            // surfaced once chained decompaction produced leading dims:
            // using the global dim made aLen too large, emitting more tuple
            // entries than the group's arity.)
            let dPos = dimArg - leadingN   // logical position within the group
            let aLen = dPos            // left group arity
            let bLen = r - dPos - 1    // right group arity
            // Build the per-group SYMM mask: a run of arity>=2 is one group
            // (compact); arity-1 (and the freed axis) are distinct singletons
            // (dense). This mirrors buildSymmVec's adjacent-equal grouping.
            let mask =
                let acc = System.Collections.Generic.List<int>()
                let mutable g = 1
                let emitGroup len =
                    if len = 1 then
                        acc.Add g
                        g <- g + 1
                    elif len > 1 then
                        for _ in 1 .. len do acc.Add g
                        g <- g + 1
                    // len <= 0: emit nothing, do NOT advance the group counter
                // Leading free dims are distinct dense singletons, emitted
                // before the fission group's mask entries.
                for _ in 1 .. leadingN do emitGroup 1
                emitGroup aLen
                emitGroup 1            // the freed axis (always a singleton)
                emitGroup bLen
                List.ofSeq acc
            let symmArg = hoistSymmDecl ($"{varName}_symm") mask
            // Total output rank = leading free dims + the fission group's
            // r expanded axes. All axes share extent n (== arrName.extents[0]).
            let totalRank = leadingN + r
            // Shared companion-extents rule -- every fission axis re-ranges over
            // the same `n`, so the table is static-constexpr exactly when that
            // one extent is pinned.
            let (extentDecl, ownedExtents) =
                emitExtentsTable "" extentsName totalRank [ for _ in 1 .. totalRank -> nDim ]
            let allocDecl =
                arrayAlloc { Ind = ""; Elem = elemTypeStr; Rank = totalRank; Name = varName
                             Symm = symmArg; Strict = None; Extents = extentsName }
            // Emit a left-justified canonical nest for a group. Returns the
            // generated loop-open lines, the storage subscript ("[v0][v1]..")
            // and the names of the per-level LOGICAL vars (prefix sums).
            let lvName tag k = $"__dc{varName}_{tag}{k}"
            let emitGroupNest (tag: string) (len: int) (startIndent: int)
                : string list * string * string list =
                let mutable lines = []
                let mutable subs = ""
                let mutable logs = []
                for k in 0 .. len - 1 do
                    let ind = String.replicate (startIndent + k) "    "
                    let v = lvName tag k
                    let logName = v + "_log"
                    let bound =
                        if k = 0 then nExpr
                        else $"""{nExpr} - {((lvName tag (k-1)) + "_log")}"""
                    let logRhs =
                        if k = 0 then v
                        else $"""{((lvName tag (k-1)) + "_log")} + {v}"""
                    lines <- lines @
                        [ forLoop ind v bound
                          $"{ind}    size_t {logName} = {logRhs};" ]
                    subs <- subs + $"[{v}]"
                    logs <- logs @ [logName]
                (lines, subs, logs)
            let fv = $"__dc{varName}_F"
            // Leading free dims become the outermost loops; the fission nest
            // is emitted indented beneath them. Their indices are prefixed
            // (leadSubs) to both the output and source addresses.
            let leadLines =
                [ for j in 0 .. leadingN - 1 ->
                    let ind = String.replicate j "    "
                    $$"""{{ind}}for (size_t {{(leadVar j)}} = 0; {{(leadVar j)}} < {{nExpr}}; {{(leadVar j)}}++) {""" ]
            let mutable depth = leadingN
            let (lLines, lSubs, lLogs) = emitGroupNest "L" aLen depth
            depth <- depth + aLen
            let fInd = String.replicate depth "    "
            let fLine = forLoop fInd fv nExpr
            depth <- depth + 1
            let (rLines, rSubs, rLogs) = emitGroupNest "R" bLen depth
            depth <- depth + bLen
            let logicalTuple = lLogs @ [fv] @ rLogs
            let bodyInd = String.replicate depth "    "
            let arrInit = logicalTuple |> String.concat ", "
            let srcSub =
                [ for k in 0 .. r - 1 ->
                    if k = 0 then $"[__dc{varName}_t[0]]"
                    else $"[__dc{varName}_t[{k}] - __dc{varName}_t[{k-1}]]" ]
                |> String.concat ""
            // Free leading dims map identically source->dest, so prefix them
            // to both subscripts.
            let outSub = leadSubs + lSubs + $"[{fv}]" + rSubs
            let srcSubFull = leadSubs + srcSub
            let body =
                [ $$"""{{bodyInd}}size_t __dc{{varName}}_t[{{r}}] = { {{arrInit}} };"""
                  $"{bodyInd}std::sort(__dc{varName}_t, __dc{varName}_t + {r});"
                  $"{bodyInd}{varName}{outSub} = {arrName}{srcSubFull};" ]
            let closes = [ for dd in depth - 1 .. -1 .. 0 -> $"""{(String.replicate dd "    ")}}}""" ]
            Some (extentDecl @ [allocDecl] @ leadLines @ lLines @ [fLine] @ rLines @ body @ closes,
                  [MatPool (varName, elemTypeStr, totalRank, symmArg, None, ownedExtents)])
         | SymAntisymmetric when r = 2 ->
            // ----- Antisym rank-2: fully dissolves to dense nxn -----
            // Zero-fill (diagonal stays 0). Walk a in [0,n), b in [0,n-a-1);
            // strict: i=a, j=a+b+1. Write +A to (i,j), -A to (j,i).
            let (extentDecl, ownedExtents) =
                emitExtentsTable "" extentsName 2 [nDim; nDim]
            let allocDecl =
                $"Array<{elemTypeStr}, 2> {varName} = {{ allocate<typename promote<{elemTypeStr}, 2>::type, nullptr>({extentsName}), {extentsName} }};"
            let a = $"__dc{varName}_a"
            let b = $"__dc{varName}_b"
            let zeroFill =
                [ $"for (size_t __dcz0 = 0; __dcz0 < {nExpr}; __dcz0++)"
                  $"    for (size_t __dcz1 = 0; __dcz1 < {nExpr}; __dcz1++)"
                  $"        {varName}[__dcz0][__dcz1] = 0;" ]
            let loops =
                [ $$"""for (size_t {{a}} = 0; {{a}} < {{nExpr}}; {{a}}++) {"""
                  $$"""    for (size_t {{b}} = 0; {{b}} + 1 < {{nExpr}} - {{a}}; {{b}}++) {"""
                  $"        size_t __dci = {a}; size_t __dcj = {a} + {b} + 1;"
                  $"        {varName}[__dci][__dcj] = {arrName}[{a}][{b}];"
                  $"        {varName}[__dcj][__dci] = -({arrName}[{a}][{b}]);"
                  "    }"
                  "}" ]
            Some (extentDecl @ [allocDecl] @ zeroFill @ loops,
                  [MatPool (varName, elemTypeStr, 2, "nullptr", None, ownedExtents)])
         | SymHermitian when r = 2 ->
            // ----- Hermitian rank-2: dissolves to dense nxn -----
            // Source is upper-triangle Hermitian storage (from gram). Walk the
            // INCLUSIVE upper triangle i<=j (diagonal kept -- it is real for a
            // Hermitian matrix, unlike the zeroed antisym diagonal): write the
            // stored value to [i][j] and its CONJUGATE to the mirror [j][i].
            // conj_scalar is std::conj on complex / identity on real, so this
            // also handles a (degenerate) real Hermitian = symmetric input.
            let (extentDecl, ownedExtents) =
                emitExtentsTable "" extentsName 2 [nDim; nDim]
            let allocDecl =
                $"Array<{elemTypeStr}, 2> {varName} = {{ allocate<typename promote<{elemTypeStr}, 2>::type, nullptr>({extentsName}), {extentsName} }};"
            let a = $"__dc{varName}_a"
            let b = $"__dc{varName}_b"
            let loops =
                [ $$"""for (size_t {{a}} = 0; {{a}} < {{nExpr}}; {{a}}++) {"""
                  $$"""    for (size_t {{b}} = 0; {{a}} + {{b}} < {{nExpr}}; {{b}}++) {"""
                  $"        size_t __dci = {a}; size_t __dcj = {a} + {b};"
                  $"        {varName}[__dci][__dcj] = {arrName}[{a}][{b}];"
                  $"        if (__dci != __dcj) {varName}[__dcj][__dci] = nested_array_utilities::conj_scalar({arrName}[{a}][{b}]);"
                  "    }"
                  "}" ]
            Some (extentDecl @ [allocDecl] @ loops,
                  [MatPool (varName, elemTypeStr, 2, "nullptr", None, ownedExtents)])
         | SymAntisymmetric when r >= 3 ->
            // ----- Antisym rank>=3: COMPACT-RESIDUAL fission (general) -----
            // decompact(anti<r>, dPos) severs the group into a chain:
            //   left residual (arity dPos) -> freed Idx -> right residual
            //   (arity r-1-dPos), the two residuals being INDEPENDENT antisym
            //   groups (NOT one merged group). Each residual of arity>=2 is a
            //   compact strict group; arity 1 degenerates to a plain Idx;
            //   arity 0 is absent. Storage is per-group strict (allocate_strict)
            //   with the mask derived from the result TYPE via
            //   buildSymmVecWithStrict. The scatter stores CANONICAL values
            //   with the FULL-tuple (cross-group + freed) sign BAKED (canon_fold
            //   over the whole logical tuple); each residual group's OWN
            //   antisymmetry is applied lazily on read. Proven end-to-end
            //   (twogroup_clean / general_scatter_emit) for boundary (one
            //   residual), one-sided interior (rank 4), and two-sided interior
            //   (rank 5) cuts. The rank-3 interior case (both residuals arity 1)
            //   is fully dense and handled by the same emission (no strict
            //   groups, two plain freed-style loops + the freed axis).
            let dPos = dimArg
            let aLen = dPos
            let bLen = r - dPos - 1
            // Per-slot descriptors in logical order: (kind, arity, startVar fn).
            // kind: "group" (strict compact, arity>=2) | "plain" (single dense
            // axis: a degenerate residual OR the freed axis).
            // Build the ordered slot list.
            let slotList =
                [ if aLen >= 2 then yield ("group", aLen)
                  elif aLen = 1 then yield ("plain", 1)
                  yield ("freed", 1)
                  if bLen >= 2 then yield ("group", bLen)
                  elif bLen = 1 then yield ("plain", 1) ]
            // Result type drives the storage mask. resultType isn't bound in
            // this arm (only the source arrTy is), so build the mask directly
            // from slotList using the same grouping rule as
            // buildSymmVecWithStrict: each arity>=2 group is one strict
            // compact group; each plain/freed axis is its own dense singleton.
            let (symmMaskVec, strictMaskVec) =
                let mutable symm = []
                let mutable strict = []
                let mutable g = 1
                for (kind, slotRank) in slotList do
                    match kind with
                    | "group" ->
                        for _ in 0 .. slotRank - 1 do
                            symm <- symm @ [g]
                            strict <- strict @ [1]
                        g <- g + 1
                    | _ ->
                        symm <- symm @ [g]
                        strict <- strict @ [0]
                        g <- g + 1
                (symm, strict)
            let symmArg = hoistSymmDecl ($"{varName}_symm") symmMaskVec
            let strictArg = hoistSymmDecl ($"{varName}_strict") strictMaskVec
            let (extentDecl, ownedExtents) =
                emitExtentsTable "" extentsName r [ for _ in 1 .. r -> nDim ]
            let allocDecl =
                arrayAlloc { Ind = ""; Elem = elemTypeStr; Rank = r; Name = varName
                             Symm = symmArg; Strict = Some strictArg; Extents = extentsName }
            // Emit the loop nest in slot order. Track:
            //   - loopLines: the for-loop openers (with indentation)
            //   - storeSubs: the storage subscript pieces (strict-relative for
            //     groups, raw var for plain/freed)
            //   - logTuple: the logical index expressions in slot order (for
            //     assembling the full tuple whose sign is baked)
            let mutable loopLines = []
            let mutable storeSubs = ""
            let mutable logTuple = []
            let mutable depth = 0
            let mutable gi = 0    // group counter (for var naming)
            let mutable pi = 0    // plain/freed counter
            for (kind, slotRank) in slotList do
                match kind with
                | "group" ->
                    // strict left-justified sub-nest of `slotRank` levels.
                    let g = gi
                    gi <- gi + 1
                    for k in 0 .. slotRank - 1 do
                        let ind = String.replicate depth "    "
                        let v = $"__dc{varName}_g{g}_{k}"
                        let logName = v + "_log"
                        let bound =
                            if k = 0 then nExpr
                            else $"""{nExpr} - {(sprintf "__dc%s_g%d_%d_log" varName g (k-1))} - 1"""
                        let logRhs =
                            if k = 0 then v
                            else $"""{(sprintf "__dc%s_g%d_%d_log" varName g (k-1))} + {v} + 1"""
                        loopLines <- loopLines @
                            [ forLoop ind v bound
                              $"{ind}    size_t {logName} = {logRhs};" ]
                        storeSubs <- storeSubs + $"[{v}]"
                        logTuple <- logTuple @ [logName]
                        depth <- depth + 1
                | _ ->
                    // "plain" (degenerate residual) or "freed": one dense axis.
                    let v = $"__dc{varName}_p{pi}"
                    pi <- pi + 1
                    let ind = String.replicate depth "    "
                    loopLines <- loopLines @
                        [ forLoop ind v nExpr ]
                    storeSubs <- storeSubs + $"[{v}]"
                    logTuple <- logTuple @ [v]
                    depth <- depth + 1
            let bodyInd = String.replicate depth "    "
            let arrInit = logTuple |> String.concat ", "
            // Source read: the source is the rank-r strict antisym storage; the
            // canonical value lives at the strict left-justified position of the
            // SORTED logical tuple. canon_fold sorts __dc_a in place (strict) and
            // yields parity + zero flag (repeat => antisym 0).
            let srcSub =
                [ for k in 0 .. r - 1 ->
                    if k = 0 then $"[__dc{varName}_t[0]]"
                    else $"[__dc{varName}_t[{k}] - __dc{varName}_t[{k-1}] - 1]" ]
                |> String.concat ""
            let body =
                [ $$"""{{bodyInd}}std::array<size_t,{{r}}> __dc{{varName}}_a = { {{arrInit}} };"""
                  $"{bodyInd}bool __dc{varName}_z; int __dc{varName}_p = nested_array_utilities::canon_fold<{r}>(__dc{varName}_a, true, __dc{varName}_z);"
                  $"""{bodyInd}size_t __dc{varName}_t[{r}] = {{ {(String.concat ", " [ for k in 0 .. r - 1 -> $"__dc{varName}_a[{k}]" ])} }};"""
                  sprintf "%s%s%s = __dc%s_z ? %s() : nested_array_utilities::canon_transform<%s>(%s%s, __dc%s_p, nested_array_utilities::ReadTransform::NegateOnSwap);"
                      bodyInd varName storeSubs varName elemTypeStr elemTypeStr arrName srcSub varName ]
            let closes = [ for dd in depth - 1 .. -1 .. 0 -> $"""{(String.replicate dd "    ")}}}""" ]
            Some (extentDecl @ [allocDecl] @ loopLines @ body @ closes,
                  [MatPool (varName, elemTypeStr, r, symmArg, Some strictArg, ownedExtents)])
         | _ -> None)
     | _ -> None)


and materializeNegateConjugateForm (subst: SubstMap) (names: Map<IRId, string>) (varName: string) (elemTypeStr: string) (form: IRExpr) (arrExpr: IRExpr) : (string list * MaterializedAlloc list) option =
    // Whole-array eager transform (negate for antisym transpose, conjugate
    // for Hermitian transpose). Type-PRESERVING: the result has the same
    // storage shape/SYMM as the source, so we allocate a fresh same-shape
    // array and run a flat contiguous-pool transform (negate_pool /
    // conjugate_pool). Every array reaching here has compact storage (one
    // contiguous pool), so pool_base + count is correct and storage-agnostic.
    let isConj = (form.IsIRArrayConjugate)
    let arrName = exprToCppCore subst names arrExpr
    let srcType = inferExprType arrExpr
    (match srcType with
     | ArrayElem arrTy ->
        let rank = arrTy.IndexTypes |> List.sumBy (fun ix -> max 1 ix.Rank)
        let extentsName = $"{varName}_extents"
        // Same-shape extents: copy the source's logical extents, through the
        // shared companion-extents rule so the result survives a return.
        let (extentDecl, ownedExtents) =
            emitExtentsTable "" extentsName rank
                [ for d in 0 .. rank - 1 -> extentDimOfArray arrTy arrName d ]
        // Allocate the destination with the SOURCE's storage class so the
        // result type is identical (antisym stays antisym, etc.).
        let spec = classifyOutputStorage srcType
        let symmArg =
            match spec with
            | AllocPerGroupStrict _ ->
                // Compact-grouped SYMM (antisym grouped like symmetric) so it
                // aligns with the STRICT mask emitAllocRhs hoists.
                let (sVec, _) = buildSymmVecWithStrict srcType
                if hasRealSymmetry sVec then hoistSymmDecl ($"{varName}_symm") sVec
                else "nullptr"
            | _ ->
                let symmVec = buildSymmVec srcType
                if hasRealSymmetry symmVec then hoistSymmDecl ($"{varName}_symm") symmVec
                else "nullptr"
        let allocRhs =
            match emitAllocRhs spec elemTypeStr rank symmArg extentsName with
            | Ok rhs -> rhs
            | Error msg -> recordCodegenRefusal msg; $"{{ nullptr, {extentsName} }};\n#error \"{msg}\""
        let allocDecl = $"Array<{elemTypeStr}, {rank}> {varName} = {allocRhs};"
        // Element count: count_antisym for antisym storage, count_leaves
        // (with the SYMM mask) otherwise. Matches the allocator's traversal.
        let countExpr =
            match spec with
            | AllocAntisymmetric ->
                // Strict storage: all-ones mask + DIAGONALS=false, same as the
                // unified allocate path.
                let allOnes = List.replicate rank 1
                let cMask = hoistSymmDecl ($"{extentsName}_anti") allOnes
                $"count_leaves<typename promote<{elemTypeStr}, {rank}>::type, {cMask}, false>({extentsName})"
            | AllocPerGroupStrict strictVec ->
                // Mixed strictness: count via the per-group-strict recurrence
                // using the same SYMM + STRICT masks the allocator used.
                let cStrict = hoistSymmDecl ($"{extentsName}_cstrict") strictVec
                $"count_leaves_strict<typename promote<{elemTypeStr}, {rank}>::type, {symmArg}, {cStrict}>({extentsName})"
            | _ ->
                // Symmetric/Hermitian/dense: DIAGONALS defaults true, DEPTH defaults 0.
                $"count_leaves<typename promote<{elemTypeStr}, {rank}>::type, {symmArg}>({extentsName})"
        let countName = $"{varName}_n"
        let routine = if isConj then "conjugate_pool" else "negate_pool"
        let call =
            [ $"size_t {countName} = {countExpr};"
              $"{routine}(pool_base({varName}.data), pool_base({arrName}.data), {countName});" ]
        // The storage class is the SOURCE's, so the free must go back through
        // deallocArgsFor with the same spec that emitAllocRhs consumed above
        // (antisym's `mask, false` triple included).
        Some (extentDecl @ [allocDecl] @ call,
              [MatPoolSpec (varName, spec, elemTypeStr, rank, symmArg, extentsName, ownedExtents)])
     | _ -> None)


and materializeArrayCopyForm (subst: SubstMap) (names: Map<IRId, string>) (varName: string) (elemTypeStr: string) (arrExpr: IRExpr) : (string list * MaterializedAlloc list) option =
    // Deep copy of an existing array into a freshly allocated same-shape,
    // same-storage buffer. Backs the COPY semantics of `let mut a = Z`
    // (IRModule.MutableArrayLets): binding the Array<T,N> wrapper by value
    // shares the data pointer, so mutations through `a` would silently
    // corrupt `Z`. Structure mirrors materializeNegateConjugateForm --
    // same-shape alloc with the source's storage class (symmetric stays
    // symmetric, etc.), count_leaves for the pool cardinality -- with the
    // transform replaced by a flat std::copy_n over the contiguous pool.
    let arrName = exprToCppCore subst names arrExpr
    let srcType = inferExprType arrExpr
    (match srcType with
     | ArrayElem arrTy ->
        let rank = arrTy.IndexTypes |> List.sumBy (fun ix -> max 1 ix.Rank)
        let extentsName = $"{varName}_extents"
        // Shared companion-extents rule, as in the negate/conjugate twin above.
        let (extentDecl, ownedExtents) =
            emitExtentsTable "" extentsName rank
                [ for d in 0 .. rank - 1 -> extentDimOfArray arrTy arrName d ]
        let spec = classifyOutputStorage srcType
        let symmArg =
            match spec with
            | AllocPerGroupStrict _ ->
                let (sVec, _) = buildSymmVecWithStrict srcType
                if hasRealSymmetry sVec then hoistSymmDecl ($"{varName}_symm") sVec
                else "nullptr"
            | _ ->
                let symmVec = buildSymmVec srcType
                if hasRealSymmetry symmVec then hoistSymmDecl ($"{varName}_symm") symmVec
                else "nullptr"
        let allocRhs =
            match emitAllocRhs spec elemTypeStr rank symmArg extentsName with
            | Ok rhs -> rhs
            | Error msg -> recordCodegenRefusal msg; $"{{ nullptr, {extentsName} }};\n#error \"{msg}\""
        let allocDecl = $"Array<{elemTypeStr}, {rank}> {varName} = {allocRhs};"
        let countExpr =
            match spec with
            | AllocAntisymmetric ->
                let allOnes = List.replicate rank 1
                let cMask = hoistSymmDecl ($"{extentsName}_anti") allOnes
                $"count_leaves<typename promote<{elemTypeStr}, {rank}>::type, {cMask}, false>({extentsName})"
            | AllocPerGroupStrict strictVec ->
                let cStrict = hoistSymmDecl ($"{extentsName}_cstrict") strictVec
                $"count_leaves_strict<typename promote<{elemTypeStr}, {rank}>::type, {symmArg}, {cStrict}>({extentsName})"
            | _ ->
                $"count_leaves<typename promote<{elemTypeStr}, {rank}>::type, {symmArg}>({extentsName})"
        let countName = $"{varName}_n"
        let call =
            [ $"size_t {countName} = {countExpr};"
              $"std::copy_n(pool_base({arrName}.data), {countName}, pool_base({varName}.data));" ]
        Some (extentDecl @ [allocDecl] @ call,
              [MatPoolSpec (varName, spec, elemTypeStr, rank, symmArg, extentsName, ownedExtents)])
     | _ -> None)


and materializeGramForm (subst: SubstMap) (names: Map<IRId, string>) (varName: string) (elemTypeStr: string) (lExpr: IRExpr) (rExpr: IRExpr) (sameArray: bool) : (string list * MaterializedAlloc list) option =
    // gram(A, B) = A * B^H:  result[i][j] = sum_k A[i][k] * conj(B[j][k]).
    // A : m x n, B : p x n.  conj() is std::conj on complex, identity on real
    // (we always emit std::conj; for real element types it is a harmless
    // no-op via the conj_scalar overload). Two modes:
    //   sameArray  -> square m x m, SymHermitian/SymSymmetric storage,
    //                 UPPER-TRIANGLE scatter only (i<=j, left-justified jr);
    //                 the lower triangle is recovered lazily on read
    //                 (canon ConjugateOnSwap for Hermitian, plain for sym).
    //   distinct   -> dense m x p, full scatter over all (i,j).
    let lName = exprToCppCore subst names lExpr
    let rName = exprToCppCore subst names rExpr
    let lTy = inferExprType lExpr
    let rTy = inferExprType rExpr
    (match lTy, rTy with
     | ArrayElem la, ArrayElem ra ->
        // element type of the result (complex iff either operand complex);
        // units ride an IRTUnitAnnotated wrapper (erased by irTypeToCpp), so
        // complex is detected on the stripped type
        let isComplexElem (t: IRType) =
            match stripUnits t with IRTScalar (ETComplex64 | ETComplex128) -> true | _ -> false
        // inferGram REFUSES operands whose element types differ (they must be
        // converted explicitly at the call site), so the two agree here and
        // this reads the agreed type off the left one. The complex arms
        // remain because a unit-annotated or otherwise wrapped pair can still
        // present differently-spelled but unified types.
        let outElem =
            if isComplexElem la.ElemType then la.ElemType
            elif isComplexElem ra.ElemType then ra.ElemType
            else la.ElemType
        let outElemStr = irTypeToCpp outElem
        // The contracted-axis extent comes from A's trailing dim: a LITERAL when
        // the operand's own index record carries one (shape monomorphization
        // pinned it), else the runtime read -- see `literalOrRuntimeExtent`. The
        // nest emitter has baked literals since Phase 4 and these sites did not,
        // which is the only reason a program with `Idx<23>` operands emitted
        // `A.extents[0]` here. Same VALUE either way; a literal is what lets GCC
        // see the trip count.
        let nExtent = literalOrRuntimeExtentOfArray la lName 1
        let mDim = extentDimOfArray la lName 0
        let pDim = extentDimOfArray ra rName 0
        let mExtent = fst mDim
        let pExtent = fst pDim
        let extentsName = $"{varName}_extents"
        // Row-pointer hoists for the contraction loop. `&X[i][0]` (not `X[i]`)
        // is the one spelling that works for every operand wrapper the arms
        // below already index with `X[i][__gk]`: `Array<T,2>::operator[]` hands
        // back a `T*`, a ragged row hands back a row wrapper, and `&row[0]` is a
        // `T*` in both. `p[__gk]` IS `*(p + __gk)`, so this is CSE, not a new
        // access pattern.
        //
        // BLADE_RESTRICT is the reason to hoist at all: it tells the optimizer
        // the two contraction operands and the (freshly allocated) output pool
        // do not overlap. NOTE the measured caveat recorded in
        // docs/plan-cpp-perf-exploitation.md: g++ feeds restrict into its
        // points-to solver for function PARAMETERS, and drops it on a
        // BLOCK-SCOPE LOCAL -- so on g++ the hoist's real payoff is the removed
        // per-iteration row load, and the qualifier is for the compilers that do
        // honour it. No BLADE_IVDEP accompanies it here: the `__gk` loop
        // accumulates into `__gacc`, which IS a loop-carried dependence, so the
        // assertion would be false (and inert -- vectorizing an FP reduction
        // needs a reassociation licence this site does not have; see the
        // byte-identity note below).
        let lRowDecl name idx =
            $"const {(irTypeToCpp la.ElemType)}* BLADE_RESTRICT {name} = &{lName}[{idx}][0];"
        let rRowDecl name idx =
            $"const {(irTypeToCpp ra.ElemType)}* BLADE_RESTRICT {name} = &{rName}[{idx}][0];"
        // conj wrapper on B's element (std::conj; identity-safe on reals via
        // conj_scalar). Use conj_scalar to keep one spelling for real/complex.
        // Reads go through the hoisted rows; the multiplication, its operand
        // order and its conjugation are untouched.
        let mulTerm lRow rRow =
            $"{lRow}[__gk] * nested_array_utilities::conj_scalar({rRow}[__gk])"
        // The dispatch decision is NOT made here. LinAlgPatterns classifies the
        // node and `shimEntryPoint` applies the BLAS availability gate; a
        // routed call emits ONE `blade_linalg::` call, and NO route emits the
        // scalar loops below -- the DEFAULT path, since BLAS is off by
        // default; it is also the single copy of this arithmetic the
        // interpreter and pinned-oracle differentials cover. `shimEntry =
        // None` means the gate is off or the route resolved Native -- NOT an
        // element-type restriction: precisionOf maps s/d/c/z and the shim
        // defines all four precision entry points, so complex and float32
        // route to BLAS exactly like f64. (An earlier comment here claimed an
        // "f64 domain"; a microkernel was built on that claim before it was
        // checked -- see src/microkernels/gram_jam_cplx.cpp.)
        //
        // Output allocation/layout is IDENTICAL on both paths (packed
        // symmetric for same-array, dense otherwise); the shim writes through
        // the row skeleton.
        //
        // WHICH BACKEND is `LinAlgPatterns.resolveNodeRoute`'s decision, not
        // this site's -- device first when BLADE_CUBLAS is on, host next,
        // native if neither. Both backends' adapters take the SAME argument
        // list (skeleton + pool capacity per operand, same order); only the
        // entry NAME and include line differ.
        let linalgCall = Blade.LinAlgPatterns.classify (IRGram (lExpr, rExpr, sameArray))
        let resolved = linalgCall |> Option.bind Blade.LinAlgPatterns.resolveNodeRoute
        let shimEntry = resolved |> Option.map snd
        let useShim = shimEntry.IsSome
        // Pool capacities for the shim's contiguity probe. See
        // `denseCellCountExpr`: the probe cannot derive these from the row
        // skeleton, and without them an n = 2 packed-symmetric operand passes
        // the row-major geometry over a pool one cell too short.
        let lCells = denseCellCountExpr lTy lName
        let rCells = denseCellCountExpr rTy rName
        match resolved with
        | Some (Blade.LinAlgPatterns.CudaBlas, _) -> (cudaLinalgUsedCell ()).Value <- true
        | Some (Blade.LinAlgPatterns.HostBlas, _) -> (linalgUsedCell ()).Value <- true
        | None -> ()
        // The dispatch marker NAMES THE BACKEND, so which machine a program
        // runs its contraction on is readable from the emitted text -- the same
        // argument the precision letter on the entry point rests on. Values
        // cannot show it: host BLAS, device cuBLAS and Blade's own loops all
        // agree to within rounding.
        let dispatchTag = dispatchMarkerTag resolved
        if sameArray then
            // square m x m, symmetric/Hermitian upper-triangle storage.
            // Extents through the shared companion rule: `gram` is the form most
            // often written as a helper's whole answer (`let g = gram(a, b); g`),
            // and a frame-local table would hand the caller a dangling shape.
            let (extentDecl, ownedExtents) =
                emitExtentsTable "" extentsName 2 [mDim; mDim]
            let symmVec = [1; 1]
            let symmArg = hoistSymmDecl ($"{varName}_symm") symmVec
            let allocDecl =
                $"Array<{outElemStr}, 2> {varName} = {{ allocate<typename promote<{outElemStr}, 2>::type, {symmArg}>({extentsName}), {extentsName} }};"
            let loop =
                match shimEntry with
                | Some entry ->
                    // One dispatch call. The shim owns staging (it probes the
                    // row skeleton for contiguity and passes the pool base
                    // straight through when it is contiguous -- which a dense
                    // operand always is), owns the packed-triangular repack,
                    // and owns the cblas-vs-native choice.
                    //
                    // BLOCK comment, not `//`: materializeInlineForm's lines are
                    // SPACE-JOINED into a single-line IIFE at expression
                    // positions, where a line comment would swallow the rest of
                    // the statement.
                    [ $"/* {dispatchTag} dispatch: gram(A, A) = A * A^T -> packed upper triangle */ {entry}({mExtent}, {nExtent}, {lName}.data, {lCells}, {varName}.data);" ]
                | None ->
                    // THREADING (see BLADE_OMP_PARALLEL_FOR_DYNAMIC in
                    // cpp/blade_portability.hpp for the spelling, and the
                    // matmul emitter below for the full soundness argument):
                    // `__gi` owns output row `%s[__gi]` exclusively, rows are
                    // disjoint, and NO summation order changes -- this is not a
                    // reassociation and needs no licence, exactly the
                    // `foldKernelBuiltinOp` situation (the arithmetic is fixed
                    // by the compiler; there is no user body whose commutativity
                    // is in question). DYNAMIC schedule because this arm is
                    // TRIANGULAR: row `__gi`'s `__gjr` span is `m - __gi`, so
                    // per-iteration work shrinks and a static split would leave
                    // the low-index threads holding most of the triangle.
                    //
                    // BUILD KNOB: a serial-emission build replaces the macro
                    // with a block-comment marker of the same line count. The
                    // macro IS the whole thread construct here (the loops
                    // themselves are unchanged and already correct serially),
                    // so nothing else about this arm moves. See
                    // `ompThreadEmissionEnabled`.
                    [ (if ompThreadEmissionEnabled () then "BLADE_OMP_PARALLEL_FOR_DYNAMIC"
                       else ompThreadsSuppressedBlockMarker ())
                      $$"""for (size_t __gi = 0; __gi < {{mExtent}}; __gi++) {"""
                      $"""    {(lRowDecl "__growi" "__gi")}"""
                      $$"""    for (size_t __gjr = 0; __gjr < {{mExtent}} - __gi; __gjr++) {"""
                      "        size_t __gj = __gi + __gjr;"
                      $"""        {(rRowDecl "__growj" "__gj")}"""
                      $"        {outElemStr} __gacc = {outElemStr}();"
                      $$"""        for (size_t __gk = 0; __gk < {{nExtent}}; __gk++) {"""
                      $"""            __gacc += {(mulTerm "__growi" "__growj")};"""
                      "        }"
                      $"        {varName}[__gi][__gjr] = __gacc;"
                      "    }"
                      "}" ]
            // NOTE: `outElemStr`, not `elemTypeStr` -- gram promotes to complex
            // when either operand is complex, and the free must name the type
            // the allocation actually used. (The BLAS staging buffers above are
            // deleted inline, so they are not tracked.)
            Some (extentDecl @ [allocDecl] @ loop,
                  [MatPool (varName, outElemStr, 2, symmArg, None, ownedExtents)])
        else
            // dense m x p
            let (extentDecl, ownedExtents) =
                emitExtentsTable "" extentsName 2 [mDim; pDim]
            // CONTRACTED-DIM RUNTIME GUARD (BL8011). The checker refuses a
            // literal disagreement between A's and B's trailing extents
            // (inferGram); inside a function over `T^2` parameters the
            // extents are the caller's and the k-loop runs to A's `n`, so a
            // shorter B was read past its row end (`[[3]]` for a 1x3 against
            // a 1x2, where the interpreter threw). Emitted only when the two
            // are not the same literal, ahead of both the shim and the
            // native loops; the same-array arm has nothing to compare.
            // BLOCK-free lines: this list may be space-joined into an IIFE.
            let contractGuard =
                match literalExtentOfArray la 1, literalExtentOfArray ra 1 with
                | Some a, Some b when a = b -> []
                | _ ->
                    let rInner = literalOrRuntimeExtentOfArray ra rName 1
                    [ $$"""if ((int64_t)({{nExtent}}) != (int64_t)({{rInner}})) {"""
                      $"    std::cerr << \"Blade runtime: gram(A, B) contracts over A's trailing axis (\" << {nExtent} << \") and B's trailing axis (\" << {rInner} << \"), which must be equal\" << std::endl;"
                      "    blade_rt::panic(\"BL8011\", \"co-iteration extent mismatch\", nullptr, 0);"
                      "}" ]
            let extentDecl = extentDecl @ contractGuard
            let allocDecl =
                $"Array<{outElemStr}, 2> {varName} = {{ allocate<typename promote<{outElemStr}, 2>::type, nullptr>({extentsName}), {extentsName} }};"
            let loop =
                match shimEntry with
                | Some entry ->
                    // The output's capacity is not inferred: it is allocated
                    // dense right above, so its pool is exactly m * p cells.
                    [ sprintf "/* %s dispatch: gram(A, B) = A * B^T -> dense */ %s(%s, %s, %s, %s.data, %s, %s.data, %s, %s.data, (%s * %s));"
                          dispatchTag entry mExtent nExtent pExtent lName lCells rName rCells varName mExtent pExtent ]
                | None ->
                    // Same threading argument as the same-array arm above, with
                    // the DEFAULT (static) schedule: this arm is rectangular, so
                    // every `__gi` costs `p * n` and a static split is already
                    // balanced. Loop ORDER is unchanged -- `A[i][k]` and
                    // `B[j][k]` are both unit-stride in `k`, so gram has nothing
                    // to gain from the i-t-j reorder matmul needs.
                    // BUILD KNOB -- see the same-array arm above.
                    // UNROLL-AND-JAM over the output axis `__gj`
                    // (docs/plans/plan-unroll-and-jam.md). The fold over `__gk`
                    // is a serial dependent chain -- one accumulator, ~0.25
                    // FMA/cycle, unvectorizable -- while `__gj` sits outside it
                    // as a perfectly independent axis contributing nothing. R
                    // output cells at a time gives R independent chains that
                    // share every `__growi[__gk]` load.
                    //
                    // BITWISE, and that is the whole reason this needs no
                    // licence: each cell keeps its own accumulator and its own
                    // ascending summation order. Jamming reinterleaves
                    // INDEPENDENT cells; it never reassociates one cell's sum.
                    // The licensed fold split -- the thing BLADE_FP_REASSOC
                    // exists for -- measured 1.00x on this very shape, because
                    // splitting the fold adds chains without adding reuse.
                    //
                    // R = 5 is the measured knee, and the reason is NOT "5
                    // scalar chains". gcc transposes the R x 4 tile and runs
                    // R-lane `vaddpd` in k-order, so the binding resource is
                    // shuffle throughput (R=4: 16, R=6: 23, R=8: 32 shuffles
                    // per iteration). That is why the curve peaks at 5-6 and
                    // falls back by 8, and why `-fno-tree-vectorize` -- which
                    // really does give R scalar chains -- is 24% SLOWER.
                    // Measured on this emitter: R=5 beats R=4 by 17% at a
                    // 257-long fold and 27% at 2003.
                    //
                    // An earlier draft capped this at 4, believing gcc
                    // contracts to `vfmadd231pd` at R >= 8 and breaks bitwise-
                    // ness under Blade's shipping `-ffp-contract=fast`. That
                    // does not reproduce on THIS emission: zero `vfmadd*` in
                    // the jammed fold body at every R up to 16, and the whole
                    // 301x303 output hashes bit-identical for R = 2..16 on
                    // full-mantissa operands at `=fast`. The contraction was a
                    // sample-major prototype artifact. Widening past 5 is a
                    // speed question here, not a correctness one.
                    //
                    // Accumulators are separate named locals, never an array:
                    // the array form spills 46-103 times and gets 3.7x where
                    // named locals spill zero and get 12.3x -- the same rule
                    // the K-lane form already follows. Verified no spills
                    // through R=8 (first spill at R=12).
                    //
                    // The remainder runs the original scalar body. When
                    // `pExtent < R` that is the ONLY body that runs, so narrow
                    // outputs emit exactly what they emitted before.
                    // R IS DERIVED FROM THE OUTPUT EXTENT, not fixed. The
                    // governing variable is `p mod R`, not the knee: a tile
                    // that does not divide `p` leaves its remainder cells to
                    // the un-jammed body at base speed, and at small `p` that
                    // remainder IS most of the work. Measured at m=4001,
                    // n=257 (cyc/MAC ratios vs the un-jammed nest):
                    //
                    //     p:      6     7     8     9    12    13    16    24    40   303
                    //     R=5: 2.40  2.00  1.77  1.63  2.52  2.26  3.12  2.55  3.81  3.47
                    //     best: 3.84  3.96  3.96  3.87  3.91  3.03  3.86  3.84  3.87  3.53
                    //     at R:    6     7     8     9     6     6     8     8     8     6
                    //
                    // A fixed R=5 gets 41-100% of the best available width,
                    // and its worst cases are exactly the extents this corpus
                    // has -- the largest gram extent anywhere in tests/ is 8,
                    // where R=5 gives 1.77x against R=8's 3.96x.
                    //
                    // The rule below reproduces the measured best at every
                    // extent above. `p <= 10` takes R = p: one tile, zero
                    // remainder, every cell jammed. Above that, the largest
                    // divisor in [4..10] does the same with a tile that still
                    // fits the register file. Only when nothing divides `p` is
                    // there a remainder to trade, and there the crossover of
                    // the 3/R chain bound against gcc's flat shuffle floor
                    // (correction 16 in src/microkernels/README.md) puts the
                    // optimum at 6 -- measured best at both p=13 and p=303.
                    //
                    // A RUNTIME extent cannot be derived, so it keeps a fixed
                    // width. That path is a genuine gamble: no single R is
                    // good at every `p mod R`, and 5 is merely the knee for
                    // gcc + double. clang wants 8 and float wants 4 -- see
                    // correction 16 -- so this constant is a local optimum of
                    // one compiler and one element type, not a fact.
                    // COMPLEX DOES NOT JAM. Measured on this emitter text at
                    // m=301 n=257 p=303, complex<double> peaks at 1.13x (R=3)
                    // and then REGRESSES -- 0.90x at R=6, 0.86x at R=8 -- so
                    // the derivation below, which happily returns R=p for
                    // p<=10, would emit a slowdown for any complex gram with
                    // p in 6..10. Worse, complex at R=2 is NOT bit-identical
                    // to the un-jammed nest (the only width where that is
                    // true), and p=2 is the most common gram extent in this
                    // corpus. A 1.13x ceiling does not buy those two hazards.
                    // `jamR = 1` makes the tile bound `__gj + 1 <= p`, which
                    // would jam every cell at width 1, so the arms below are
                    // gated on `doJam` instead and complex emits the plain
                    // nest it emitted before ae951eb.
                    let doJam = not (isComplexElem outElem)
                    let jamR =
                        let runtimeKnee = 5
                        if not (snd pDim) then runtimeKnee
                        else
                            match System.Int32.TryParse(fst pDim) with
                            | true, pLit when pLit >= 2 ->
                                if pLit <= 10 then pLit
                                else
                                    match [ 10 .. -1 .. 4 ] |> List.tryFind (fun d -> pLit % d = 0) with
                                    | Some d -> d
                                    | None -> 6
                            | _ -> runtimeKnee
                    let accName k = $"__gacc{k}"
                    let rowName k = $"__growj{k}"
                    // NOTE: every element below is an EXPLICIT `yield`.
                    // The `if doJam` arm forces it: introducing one
                    // explicit yield turns OFF F#'s implicit yields for
                    // the whole list, and the bare `sprintf` lines then
                    // compile to discarded expressions (FS3221) -- i.e.
                    // silently missing lines in the emitted C++.
                    [ yield (if ompThreadEmissionEnabled () then "BLADE_OMP_PARALLEL_FOR"
                             else ompThreadsSuppressedBlockMarker ())
                      yield $$"""for (size_t __gi = 0; __gi < {{mExtent}}; __gi++) {"""
                      yield $"""    {(lRowDecl "__growi" "__gi")}"""
                      yield "    size_t __gj = 0;"
                      if doJam then
                          yield $$"""    for (; __gj + {{jamR}} <= {{pExtent}}; __gj += {{jamR}}) {"""
                          yield! [ for k in 0 .. jamR - 1 ->
                                     $"""        {(rRowDecl (rowName k) (sprintf "__gj + %d" k))}""" ]
                          yield! [ for k in 0 .. jamR - 1 ->
                                     $"        {outElemStr} {(accName k)} = {outElemStr}();" ]
                          yield $$"""        for (size_t __gk = 0; __gk < {{nExtent}}; __gk++) {"""
                          yield! [ for k in 0 .. jamR - 1 ->
                                     $"""            {(accName k)} += {(mulTerm "__growi" (rowName k))};""" ]
                          yield "        }"
                          yield! [ for k in 0 .. jamR - 1 ->
                                     $"        {varName}[__gi][__gj + {k}] = {(accName k)};" ]
                          yield "    }"
                      yield $$"""    for (; __gj < {{pExtent}}; __gj++) {"""
                      yield $"""        {(rRowDecl "__growj" "__gj")}"""
                      yield $"        {outElemStr} __gacc = {outElemStr}();"
                      yield $$"""        for (size_t __gk = 0; __gk < {{nExtent}}; __gk++) {"""
                      yield $"""            __gacc += {(mulTerm "__growi" "__growj")};"""
                      yield "        }"
                      yield $"        {varName}[__gi][__gj] = __gacc;"
                      yield "    }"
                      yield "}" ]
            Some (extentDecl @ [allocDecl] @ loop,
                  [MatPool (varName, outElemStr, 2, "nullptr", None, ownedExtents)])
     | _ -> None)


and materializeGramApplyForm (subst: SubstMap) (names: Map<IRId, string>) (varName: string) (elemTypeStr: string) (lExpr: IRExpr) (rExpr: IRExpr) (xExpr: IRExpr) : (string list * MaterializedAlloc list) option =
    // gram_apply(A, B, x) = A * (B^H * x):  y[i] = sum_k A[i][k] * t[k], with
    // t[k] = sum_j conj(B[j][k]) * x[j]. A : m x n, B : p x n, x : p. TWO
    // rank-1 pools (t: n cells, y: m cells) and no m x p matrix -- the action
    // of gram(A, B) declared as such (docs/plans/structural/05, 3.2). Both
    // halves fold ASCENDING from the element zero, one accumulator per output
    // cell, so the per-cell operation sequence is fixed and the interpreter
    // twin (gramApplyArray) reproduces it bit for bit; no reassociation, no
    // licence. The first half walks B's rows in order (unit-stride reads, the
    // n accumulators live in the t pool); the second half's rows are
    // independent and thread without a licence, exactly as gram's nest does.
    // Dispatch (BLAS on) is two L2 calls resolved by LinAlgPatterns like
    // every node route: the transposed gemv adapter for t, the plain one for
    // y; no route emits the loops below. Block comments only: this list may
    // be space-joined into a single-line IIFE at an expression position.
    let lName = exprToCppCore subst names lExpr
    let rName = exprToCppCore subst names rExpr
    let xName = exprToCppCore subst names xExpr
    let lTy = inferExprType lExpr
    let rTy = inferExprType rExpr
    let xTy = inferExprType xExpr
    (match lTy, rTy, xTy with
     | ArrayElem la, ArrayElem ra, ArrayElem xa ->
        let isComplexElem (t: IRType) =
            match stripUnits t with IRTScalar (ETComplex64 | ETComplex128) -> true | _ -> false
        let outElem =
            if isComplexElem la.ElemType then la.ElemType
            elif isComplexElem ra.ElemType then ra.ElemType
            elif isComplexElem xa.ElemType then xa.ElemType
            else la.ElemType
        let outElemStr = irTypeToCpp outElem
        let mExtent = literalOrRuntimeExtentOfArray la lName 0
        let nExtent = literalOrRuntimeExtentOfArray la lName 1
        let pExtent = literalOrRuntimeExtentOfArray ra rName 0
        let mDim = extentDimOfArray la lName 0
        let nDim = extentDimOfArray la lName 1
        let tName = $"{varName}__t"
        let tExtentsName = $"{tName}_extents"
        let extentsName = $"{varName}_extents"
        // Runtime twins of the checker's static agreement (BL8011): B's
        // trailing axis against A's, x's extent against B's leading axis.
        // Emitted only when the two are not the same literal.
        let guardIf (litA: int64 option) (litB: int64 option) (lhs: string) (rhs: string) (what: string) =
            match litA, litB with
            | Some a, Some b when a = b -> []
            | _ ->
                [ $$"""if ((int64_t)({{lhs}}) != (int64_t)({{rhs}})) {"""
                  $"    std::cerr << \"Blade runtime: gram_apply(A, B, x): {what} (\" << {lhs} << \" vs \" << {rhs} << \")\" << std::endl;"
                  "    blade_rt::panic(\"BL8011\", \"co-iteration extent mismatch\", nullptr, 0);"
                  "}" ]
        let guards =
            guardIf (literalExtentOfArray la 1) (literalExtentOfArray ra 1) nExtent (literalOrRuntimeExtentOfArray ra rName 1) "A's and B's trailing axes must be equal"
            @ guardIf (literalExtentOfArray ra 0) (literalExtentOfArray xa 0) pExtent (literalOrRuntimeExtentOfArray xa xName 0) "x must have B's leading extent"
        let (tExtentDecl, tOwned) = emitExtentsTable "" tExtentsName 1 [nDim]
        let (yExtentDecl, yOwned) = emitExtentsTable "" extentsName 1 [mDim]
        let tAlloc = $"Array<{outElemStr}, 1> {tName} = {{ allocate<typename promote<{outElemStr}, 1>::type, nullptr>({tExtentsName}), {tExtentsName} }};"
        let yAlloc = $"Array<{outElemStr}, 1> {varName} = {{ allocate<typename promote<{outElemStr}, 1>::type, nullptr>({extentsName}), {extentsName} }};"
        let halves = Blade.LinAlgPatterns.classifyGramApply lExpr rExpr xExpr
        let resolvedT = halves |> Option.bind (fun (t, _) -> Blade.LinAlgPatterns.resolveNodeRoute t)
        let resolvedY = halves |> Option.bind (fun (_, y) -> Blade.LinAlgPatterns.resolveNodeRoute y)
        let lCells = denseCellCountExpr lTy lName
        let rCells = denseCellCountExpr rTy rName
        for resolved in [ resolvedT; resolvedY ] do
            match resolved with
            | Some (Blade.LinAlgPatterns.CudaBlas, _) -> (cudaLinalgUsedCell ()).Value <- true
            | Some (Blade.LinAlgPatterns.HostBlas, _) -> (linalgUsedCell ()).Value <- true
            | None -> ()
        let tLoop =
            match resolvedT with
            | Some (_, entry) ->
                [ $"/* {(dispatchMarkerTag resolvedT)} dispatch: gram_apply(A, B, x), t = B^H x */ {entry}({pExtent}, {nExtent}, {rName}.data, {rCells}, {xName}.data, {tName}.data);" ]
            | None ->
                [ $$"""for (size_t __gk = 0; __gk < {{nExtent}}; __gk++) { {{tName}}[__gk] = {{outElemStr}}(); }"""
                  $$"""for (size_t __gj = 0; __gj < {{pExtent}}; __gj++) {"""
                  $"    const {(irTypeToCpp ra.ElemType)}* BLADE_RESTRICT __growj = &{rName}[__gj][0];"
                  $"    const {(irTypeToCpp xa.ElemType)} __gx = {xName}[__gj];"
                  $$"""    for (size_t __gk = 0; __gk < {{nExtent}}; __gk++) {"""
                  $"        {tName}[__gk] += nested_array_utilities::conj_scalar(__growj[__gk]) * __gx;"
                  "    }"
                  "}" ]
        let yLoop =
            match resolvedY with
            | Some (_, entry) ->
                [ $"/* {(dispatchMarkerTag resolvedY)} dispatch: gram_apply(A, B, x), y = A t */ {entry}({mExtent}, {nExtent}, {lName}.data, {lCells}, {tName}.data, {varName}.data);" ]
            | None ->
                [ (if ompThreadEmissionEnabled () then "BLADE_OMP_PARALLEL_FOR"
                   else ompThreadsSuppressedBlockMarker ())
                  $$"""for (size_t __gi = 0; __gi < {{mExtent}}; __gi++) {"""
                  $"    const {(irTypeToCpp la.ElemType)}* BLADE_RESTRICT __growi = &{lName}[__gi][0];"
                  $"    {outElemStr} __gacc = {outElemStr}();"
                  $$"""    for (size_t __gk = 0; __gk < {{nExtent}}; __gk++) {"""
                  $"        __gacc += __growi[__gk] * {tName}[__gk];"
                  "    }"
                  $"    {varName}[__gi] = __gacc;"
                  "}" ]
        Some (guards @ tExtentDecl @ [ tAlloc ] @ tLoop @ yExtentDecl @ [ yAlloc ] @ yLoop,
              [ MatPool (tName, outElemStr, 1, "nullptr", None, tOwned)
                MatPool (varName, outElemStr, 1, "nullptr", None, yOwned) ])
     | _ -> None)

and materializeMatmulForm (subst: SubstMap) (names: Map<IRId, string>) (varName: string) (elemTypeStr: string) (lExpr: IRExpr) (rExpr: IRExpr) : (string list * MaterializedAlloc list) option =
    // matmul(A, B) = A * B:  result[i][j] = sum_t A[i][t] * B[t][j].
    // A : m x k, B : k x n -> DENSE m x n. No conjugation, no symmetry claim
    // (A*A is not symmetric), so unlike gram there is exactly one mode.
    //
    // The typechecker restricts both operands to real Float64 (inferMatmul),
    // so the shape is always inside the shim's domain and `shimEntry` is
    // decided purely by the BLAS AVAILABILITY GATE
    // (`LinAlgPatterns.shimEntryPoint`). Gate off -- the default -- emits the
    // triple loop below; gate on emits the dispatch call.
    //
    // BYTE-IDENTITY: for each output cell (i, j) the summands are added in
    // ASCENDING t, starting from the element zero. That is the obligation --
    // `Interp/ArrayOps.matmulArray` does the same, and this is the ONLY copy of
    // that arithmetic in the system (the shim carries no fallback), so
    // `interp math` tests the very code an ordinary build runs.
    //
    // The loop below is emitted i-t-j, NOT i-j-t, and the obligation SURVIVES
    // the reorder -- this note is updated, not deleted, because the reorder is
    // exactly the change it exists to police:
    //
    //   * WHAT THE OBLIGATION CONSTRAINS is the per-cell sequence of FP
    //     additions. In i-j-t that sequence is `acc = 0; acc += a[i][0]*b[0][j];
    //     acc += a[i][1]*b[1][j]; ...`. In i-t-j it is `C[i][j] = 0;
    //     C[i][j] += a[i][0]*b[0][j]; C[i][j] += a[i][1]*b[1][j]; ...` -- the
    //     SAME operations on the SAME values in the SAME order. Only the
    //     accumulator's STORAGE moved, from a named local to the output cell it
    //     was going to be stored into anyway.
    //   * WHAT THE REORDER CHANGES is the INTERLEAVING of independent cells:
    //     cell (i, j) and cell (i, j') now advance in lockstep instead of one
    //     finishing before the other starts. Independent cells never interact,
    //     so no cell's value depends on the interleaving.
    //   * There is no `-ffast-math` anywhere in `Build.fs`, so GCC may not
    //     reassociate either form on its own. The zero-init is `T()`, the same
    //     seed the accumulator had.
    //
    // This is a reorder, NOT a reassociation. Anything that DID reassociate
    // (threading `t`, lane-splitting the contraction) would need a licence and
    // would break these differentials; nothing here does.
    //
    // MEASURED, because the argument above is about ADDITION ORDER and FP
    // contraction is a separate axis. 23x29 @ 29x31 over deliberately
    // non-representable data (1/3, 1/7, e, pi, ...), printed at 17 digits,
    // i-j-t vs i-t-j:
    //
    //   -ffp-contract=off   : byte-identical, all 713 cells.  <-- the obligation
    //   -ffp-contract=fast  : 130 of 713 cells differ, max relative 3.2e-16
    //                         (1-2 ulp), i.e. purely WHERE GCC placed its FMAs.
    //
    // The first line is the one that matters, and it is not a weaker result --
    // it is the SAME regime the byte-identity gate runs in: `tests/InterpDiff.fs`
    // and `tests/DiffOracle.fs` PIN `BLADE_FP_CONTRACT=off` for their own runs,
    // because `src/Interp/Numerics.fs` is bit-pinned to non-FMA scalar semantics
    // (see the header of `src/Build.fs`: "byte-identity is a property of the
    // differential gates, not of user builds"). Under that pin the reorder is
    // exact, which is precisely what proves only the addition ORDER was at stake.
    //
    // Under `=fast` no loop form promises a particular contraction, and master's
    // i-j-t form does not either: the same probe shows master's OWN output
    // changing under `-fno-tree-vectorize` at `=fast`. Contraction placement is
    // a build-flag property (`BLADE_FP_CONTRACT`), not an emitter contract.
    // On the data class the corpus differentials actually use -- integer-valued
    // f64, where every product and partial sum is exact -- FMA and mul+add
    // coincide, so all four combinations above agree bit for bit.
    let lName = exprToCppCore subst names lExpr
    let rName = exprToCppCore subst names rExpr
    let lTy = inferExprType lExpr
    let rTy = inferExprType rExpr
    (match lTy, rTy with
     | ArrayElem la, ArrayElem ra ->
        let outElemStr = irTypeToCpp la.ElemType
        // Literal extents when the operands' own index records carry them --
        // see `literalOrRuntimeExtent`; same value as the runtime read.
        let mDim = extentDimOfArray la lName 0
        let nDim = extentDimOfArray ra rName 1
        let mExtent = fst mDim
        let kExtent = literalOrRuntimeExtentOfArray la lName 1
        let nExtent = fst nDim
        let extentsName = $"{varName}_extents"
        // Pool capacities for the shim's contiguity probe -- see
        // `denseCellCountExpr` and the note in materializeGramForm.
        let lCells = denseCellCountExpr lTy lName
        let rCells = denseCellCountExpr rTy rName
        // ROUND D: the backend is `resolveNodeRoute`'s decision -- see the same
        // note in materializeGramForm. Both adapters take the same arguments,
        // so only the entry NAME and the include line differ.
        let linalgCall = Blade.LinAlgPatterns.classify (IRMatmul (lExpr, rExpr))
        let resolved = linalgCall |> Option.bind Blade.LinAlgPatterns.resolveNodeRoute
        let shimEntry = resolved |> Option.map snd
        match resolved with
        | Some (Blade.LinAlgPatterns.CudaBlas, _) -> (cudaLinalgUsedCell ()).Value <- true
        | Some (Blade.LinAlgPatterns.HostBlas, _) -> (linalgUsedCell ()).Value <- true
        | None -> ()
        // Shared companion-extents rule -- see the note in materializeGramForm.
        let (extentDecl, ownedExtents) =
            emitExtentsTable "" extentsName 2 [mDim; nDim]
        let allocDecl =
            $"Array<{outElemStr}, 2> {varName} = {{ allocate<typename promote<{outElemStr}, 2>::type, nullptr>({extentsName}), {extentsName} }};"
        let loop =
            match shimEntry with
            | Some entry ->
                // Block comment, not `//` -- see the note in materializeGramForm.
                // As in gram-distinct: the output is allocated dense right
                // above, so its pool is exactly m * n cells.
                [ sprintf "/* %s dispatch: matmul(A, B) = A * B -> dense */ %s(%s, %s, %s, %s.data, %s, %s.data, %s, %s.data, (%s * %s));"
                      (dispatchMarkerTag resolved) entry mExtent kExtent nExtent lName lCells rName rCells varName mExtent nExtent ]
            | None ->
                // i-t-j, accumulating in place. Two independent wins over
                // i-j-t, both free (see the BYTE-IDENTITY note at the top of
                // this function for why the reorder costs nothing numerically):
                //
                //  1. STRIDE. The old inner loop read `B[__mt][__mj]` with
                //     `__mt` varying -- a walk DOWN a column, one cache line
                //     touched per element. Here `__mj` varies innermost over a
                //     hoisted row `__mbrow`, so both the read and the write are
                //     unit-stride and the loop is vectorizable at all.
                //  2. THREADS. `__mi` owns output row `__mcrow` exclusively;
                //     rows are disjoint and no cell's summation order changes,
                //     so the outer level is threaded with no licence needed --
                //     the arithmetic here is the COMPILER'S, not a user kernel's
                //     (the `foldKernelBuiltinOp` situation). The static schedule
                //     is right because every `__mi` costs exactly k*n.
                //     Threading `__mt` instead WOULD reassociate; it is not done.
                //
                // BLADE_IVDEP on the `__mj` loop is a TRUE assertion, not the
                // decorative kind: `__mcrow` points into `%s`, freshly allocated
                // immediately above and therefore a distinct pool from both
                // operands, so there is no loop-carried dependence across `__mj`
                // at all. (The gram arms get no ivdep -- their inner loop is a
                // reduction. See the note there.)
                //
                // BUILD KNOB: a serial-emission build swaps the macro for a
                // block-comment marker. `BLADE_IVDEP` on the `__mj` loop below
                // is UNTOUCHED -- it is a vectorization assertion, not a thread
                // construct, and the knob is about teams. See
                // `ompThreadEmissionEnabled`.
                [ (if ompThreadEmissionEnabled () then "BLADE_OMP_PARALLEL_FOR"
                   else ompThreadsSuppressedBlockMarker ())
                  $$"""for (size_t __mi = 0; __mi < {{mExtent}}; __mi++) {"""
                  $"    {outElemStr}* BLADE_RESTRICT __mcrow = &{varName}[__mi][0];"
                  $$"""    for (size_t __mj = 0; __mj < {{nExtent}}; __mj++) { __mcrow[__mj] = {{outElemStr}}(); }"""
                  $$"""    for (size_t __mt = 0; __mt < {{kExtent}}; __mt++) {"""
                  $"        const {outElemStr} __ma = {lName}[__mi][__mt];"
                  $"        const {(irTypeToCpp ra.ElemType)}* BLADE_RESTRICT __mbrow = &{rName}[__mt][0];"
                  "        BLADE_IVDEP"
                  $$"""        for (size_t __mj = 0; __mj < {{nExtent}}; __mj++) {"""
                  "            __mcrow[__mj] += __ma * __mbrow[__mj];"
                  "        }"
                  "    }"
                  "}" ]
        Some (extentDecl @ [allocDecl] @ loop,
              [MatPool (varName, outElemStr, 2, "nullptr", None, ownedExtents)])
     | _ -> None)


and materializeEighForm (subst: SubstMap) (names: Map<IRId, string>) (varName: string) (operand: IRExpr) : (string list * MaterializedAlloc list) option =
    // eigh(S) -> (Q, LAM). Unlike other materialize*Form (ONE array), this
    // produces a TUPLE of two arrays with (for complex) DIFFERENT element
    // types; `let (Q, LAM) = m.eigh(S)` already lowers to an anonymous tuple
    // binding + two `IRTupleProj`s, which codegen emits as
    //     std::tuple<Array<double, 2>, Array<double, 1>> __tup_58 = <value>;
    //     Array<double, 2> Q   = std::get<0>(__tup_58);
    //     Array<double, 1> LAM = std::get<1>(__tup_58);
    // so the novelty is confined to producing that one `<value>`.
    //
    // Shape: declare two pools under derived names (`<var>__q`, `<var>__lam`),
    // let the shim write into them, then bind `varName` to
    // `std::make_tuple(<var>__q, <var>__lam)` (copies of the two wrappers,
    // aliasing one pool/extents table each; both returned as MatPool
    // descriptors so scope exit frees them). The shim writes into
    // pre-allocated pools because `blade_lapack` takes `lam`/`V` as OUT
    // params -- LAPACK's workspace never escapes the adapter. Q is DENSE even
    // for a packed operand: eigenvectors carry no symmetry of their own.
    //
    // Route via `LinAlgPatterns.classifyEigh`: packed hands LAPACK
    // `pool_base(S.data)` with ZERO conversion (Blade's row-major-upper packed
    // pool IS col-major-lower packed for symmetric); dense hands it the row
    // SKELETON `S.data`, copied through `Arows[i][j]`.
    let srcName = exprToCppCore subst names operand
    (match inferExprType operand with
     | ArrayElem sa ->
        let call = Blade.LinAlgPatterns.classifyEigh sa
        let shimEntry = call |> Option.bind (Blade.LinAlgPatterns.shimEntryPoint Blade.LinAlgPatterns.HostBlas)
        match call, shimEntry with
        | Some c, Some entry ->
            (lapackUsedCell ()).Value <- true
            let qElemStr = irTypeToCpp sa.ElemType
            // The eigenvalues of a symmetric/Hermitian matrix are REAL, so LAM's
            // element type is the operand's REAL counterpart -- `double` beside a
            // `std::complex<double>` Q. This mirrors `IR.CarriedType`'s IREigh
            // arm and `TypeCheck.inferEigh`'s result type; all three must agree,
            // and the shim's signature (`std::complex<double>** V, double* lam`)
            // is the fourth statement of the same fact.
            let lamElemStr =
                match sa.ElemType with
                | IRTScalar ETComplex128 -> irTypeToCpp (IRTScalar ETFloat64)
                | IRTScalar ETComplex64 -> irTypeToCpp (IRTScalar ETFloat32)
                | t -> irTypeToCpp t
            let nExtent = $"{srcName}.extents[0]"
            let qName = $"{varName}__q"
            let lamName = $"{varName}__lam"
            let qExtents = $"{qName}_extents"
            let lamExtents = $"{lamName}_extents"
            // `n` COMES FROM `.extents[0]`, AND THAT IS RIGHT FOR BOTH ROUTES --
            // measured, not assumed. A rank-2 compact pool's extents table holds
            // the LOGICAL extents, not the packed cell count: `gram(A, A)`
            // emits `G_extents[0] = A.extents[0]; G_extents[1] = A.extents[0];`
            // for its packed symmetric result. So `S.extents[0]` is `n` whether
            // S is dense or packed, and the shim derives `n(n+1)/2` itself.
            //
            // THE PACKED ROUTE IS NOT REACHABLE FROM `math.eigh` TODAY, and the
            // block is kept anyway. `MathElaborate.arrayShape` resolves a
            // declared shape one plain axis at a time, so a `SymIdx<2, n>`
            // operand is refused with BL5200 ("every axis extent must be
            // statically known") BEFORE any marker is emitted -- identically with
            // the gate on and off, so this introduces no gate-dependent
            // asymmetry. That makes packed eigh the same shape `blade_symv`
            // already has: a verified route (classifier pins + the shim's
            // invariant checks) waiting on a surface. Teaching `arrayShape`
            // compact axes is the one change that lands it.
            //
            // The operand as the shim wants it: a flat packed pool for the
            // `?spev`/`?hpev` route, the row skeleton for `?syev`/`?heev`.
            let (operandArg, routeLabel) =
                match c.Route with
                | Blade.LinAlgPatterns.RouteEighPacked ->
                    ($"nested_array_utilities::pool_base({srcName}.data)",
                     "packed upper-triangular operand, zero conversion")
                | _ ->
                    ($"{srcName}.data", "dense operand, symmetry asserted")
            // Both tables go through the shared companion-extents rule. `n` is
            // the runtime `.extents[0]` read argued for above, so both land on
            // the heap arm -- which is also what lets a destructured `Q` or
            // `LAM` be returned out of the body that computed the pair.
            let (qExtentsDecl, qOwnedExtents) =
                emitExtentsTable "" qExtents 2 [(nExtent, false); (nExtent, false)]
            let (lamExtentsDecl, lamOwnedExtents) =
                emitExtentsTable "" lamExtents 1 [(nExtent, false)]
            let decls =
                qExtentsDecl
                @ [ arrayAlloc { Ind = ""; Elem = qElemStr; Rank = 2; Name = qName
                                 Symm = "nullptr"; Strict = None; Extents = qExtents } ]
                @ lamExtentsDecl
                @ [ arrayAlloc { Ind = ""; Elem = lamElemStr; Rank = 1; Name = lamName
                                 Symm = "nullptr"; Strict = None; Extents = lamExtents } ]
            // BLOCK comment, not `//`: these lines are SPACE-JOINED into a
            // single-line IIFE at expression positions, where a line comment
            // would swallow the rest of the statement (the lesson math/057
            // taught the gram emitter).
            let dispatch =
                $"/* lapack dispatch: eigh(S) -> (Q, LAM), {routeLabel} */ {entry}({nExtent}, {operandArg}, {lamName}.data, {qName}.data);"
            // The binding value the existing destructuring consumes. Spelled
            // with the EXPLICIT tuple type rather than `auto` so it matches the
            // `std::tuple<Array<double, 2>, Array<double, 1>>` form a
            // tuple-returning function's result already binds as -- one shape for
            // both producers.
            let tupleLine =
                $"std::tuple<Array<{qElemStr}, 2>, Array<{lamElemStr}, 1>> {varName} = std::make_tuple({qName}, {lamName});"
            // BOTH POOLS ARE REGISTERED, safe via the OWNER-ID path, not
            // return-NAME matching: eigh's pools aren't named after the
            // binding (`<binding>__q`/`__lam`), so `genFuncBodyScoped`'s return
            // suppression (a whole-token match against `registeredAllocNames
            // ()`) can't see them -- what escapes is the DESTRUCTURED
            // projection (`return Q;`), naming neither pool.
            //
            // The escape ANALYSIS covers it instead: `registerAlloc` STAMPS
            // every registration with the enclosing frame's `CurrentOwner`
            // (the `let` being rendered), so both pools end up owned by the
            // `__tup_N` binding. `computeScopeEscapes` reaches that id when a
            // projection escapes (`Q`'s id is seeded from `IRTupleProj(__tup,
            // 0)`, a non-barrier view form, pulling in `__tup`'s id), and
            // `popAllocScopeFrees` spares both pools together via
            // `ownerEscapes` -- ownership is all-or-nothing per let, exactly
            // the granularity a tuple of two pools needs.
            //
            // At module level there is no live frame, so registration no-ops
            // and the pools live to program exit like any other module binding.
            Some (decls @ [dispatch; tupleLine],
                  [ MatPool (qName, qElemStr, 2, "nullptr", None, qOwnedExtents)
                    MatPool (lamName, lamElemStr, 1, "nullptr", None, lamOwnedExtents) ])
        | _ ->
            // UNREACHABLE from the surface: `TypeCheck.inferEigh` accepts an
            // operand only when `classifyEigh` gave it a route, and the node
            // itself only exists when `lapackAvailable ()` held at elaboration.
            // Reaching here means the gate flipped BETWEEN elaboration and
            // emission inside one process (a test's use-guard, say). There is no
            // native arm to fall back to -- the native math is the synthesized
            // Jacobi source the elaborator did not emit -- so refuse loudly at
            // C++ compile time rather than declaring nothing and leaving the
            // destructuring to reference an undefined name.
            Some ([ refusalErrorLine "" "Blade codegen: eigh reached emission with no LAPACK route (availability gate changed after elaboration?); the synthesized Jacobi path is chosen at elaboration time and cannot be recovered here" ], [])
     | _ -> None)


and materializeLuForm (subst: SubstMap) (names: Map<IRId, string>) (varName: string) (mExpr: IRExpr) : (string list * MaterializedAlloc list) option =
    // lu(A) -> (LU, piv): the partial-pivoted LU `materializeSolveForm`
    // computes, KEPT. LU is a fresh n x n dense pool holding L (unit lower,
    // multipliers below the diagonal) and U (on and above); piv is a fresh
    // n-cell Int64 pool, piv[k] = the row swapped with row k at step k
    // (0-based, LAPACK's ipiv less one). The elimination is the solve form's
    // statement for statement -- same working-copy order, same STRICT `>`
    // pivot rule, same exact-zero singularity test (BL8007) -- so
    // `lu_solve(lu(A), b)` reproduces `solve(A, b)` to the bit, and the
    // interpreter twin `luArrays` reproduces this. Routed arm: `?getrf`
    // through `blade_lapack::blade_lu_d`, which bridges the column-major
    // scratch both ways and converts the pivots; that arm agrees to ~1e-14
    // like every LAPACK route, so the differential harnesses run gate-off.
    let aName = exprToCppCore subst names mExpr
    (match inferExprType mExpr with
     | ArrayElem aa ->
        let elemStr = irTypeToCpp aa.ElemType
        let nExtent = $"{aName}.extents[0]"
        let nName = $"{varName}__n"
        let luName = $"{varName}__lu"
        let pivName = $"{varName}__piv"
        let infoName = $"{varName}__info"
        let luExtents = $"{luName}_extents"
        let pivExtents = $"{pivName}_extents"
        let call = Blade.LinAlgPatterns.classifyLu aa
        let shimEntry = call |> Option.bind (Blade.LinAlgPatterns.shimEntryPoint Blade.LinAlgPatterns.HostBlas)
        match shimEntry with
        | Some _ -> (lapackUsedCell ()).Value <- true
        | None -> ()
        let (luExtentsDecl, luOwned) = emitExtentsTable "" luExtents 2 [(nExtent, false); (nExtent, false)]
        let (pivExtentsDecl, pivOwned) = emitExtentsTable "" pivExtents 1 [(nExtent, false)]
        let decls =
            luExtentsDecl
            @ [ arrayAlloc { Ind = ""; Elem = elemStr; Rank = 2; Name = luName; Symm = "nullptr"; Strict = None; Extents = luExtents } ]
            @ pivExtentsDecl
            @ [ arrayAlloc { Ind = ""; Elem = "int64_t"; Rank = 1; Name = pivName; Symm = "nullptr"; Strict = None; Extents = pivExtents } ]
        let panicLine = "blade_rt::panic(\"BL8007\", \"lu(A): the matrix is SINGULAR -- LU factorization found an exactly-zero pivot\", nullptr, 0);"
        let body =
            match shimEntry with
            | Some entry ->
                [ $"const size_t {nName} = {nExtent};"
                  $"/* lapack dispatch: lu(A) -> (LU, piv), dense square operand */ int {infoName} = {entry}({nName}, {aName}.data, {luName}.data, {pivName}.data);"
                  $$"""if ({{infoName}} != 0) { {{panicLine}} }""" ]
            | None ->
                [ "/* lu factor: partial-pivoted, multipliers stored below the diagonal, pivot rows in piv */"
                  $"const size_t {nName} = {nExtent};"
                  $$"""for (size_t __si = 0; __si < {{nName}}; __si++) { for (size_t __sj = 0; __sj < {{nName}}; __sj++) { {{luName}}[__si][__sj] = {{aName}}[__si][__sj]; } }"""
                  $$"""for (size_t __sk = 0; __sk < {{nName}}; __sk++) {"""
                  "    size_t __sp = __sk;"
                  $"    {elemStr} __sbig = std::fabs({luName}[__sk][__sk]);"
                  $$"""    for (size_t __si = __sk + 1; __si < {{nName}}; __si++) {"""
                  $"        {elemStr} __sm = std::fabs({luName}[__si][__sk]);"
                  "        if (__sm > __sbig) { __sbig = __sm; __sp = __si; }"
                  "    }"
                  $$"""    if ({{luName}}[__sp][__sk] == {{elemStr}}(0)) { {{panicLine}} }"""
                  $"    {pivName}[__sk] = (int64_t)__sp;"
                  "    if (__sp != __sk) {"
                  $$"""        for (size_t __sj = 0; __sj < {{nName}}; __sj++) { {{elemStr}} __st = {{luName}}[__sk][__sj]; {{luName}}[__sk][__sj] = {{luName}}[__sp][__sj]; {{luName}}[__sp][__sj] = __st; }"""
                  "    }"
                  $$"""    for (size_t __si = __sk + 1; __si < {{nName}}; __si++) {"""
                  $"        {elemStr} __sf = {luName}[__si][__sk] / {luName}[__sk][__sk];"
                  $"        {luName}[__si][__sk] = __sf;"
                  $$"""        for (size_t __sj = __sk + 1; __sj < {{nName}}; __sj++) { {{luName}}[__si][__sj] = {{luName}}[__si][__sj] - __sf * {{luName}}[__sk][__sj]; }"""
                  "    }"
                  "}" ]
        let tupleLine =
            $"std::tuple<Array<{elemStr}, 2>, Array<int64_t, 1>> {varName} = std::make_tuple({luName}, {pivName});"
        Some (decls @ ["{"] @ body @ ["}"; tupleLine],
              [ MatPool (luName, elemStr, 2, "nullptr", None, luOwned)
                MatPool (pivName, "int64_t", 1, "nullptr", None, pivOwned) ])
     | _ -> None)

and materializeLuSolveForm (subst: SubstMap) (names: Map<IRId, string>) (varName: string) (elemTypeStr: string) (lExpr: IRExpr) (pExpr: IRExpr) (rExpr: IRExpr) (transposed: bool) : (string list * MaterializedAlloc list) option =
    // lu_solve(LU, piv, b) -> x with A x = b from the stored factors: x = b,
    // the pivot swaps applied in step order, the multipliers applied in the
    // solve form's fused order (k outer, i inner), then the same back
    // substitution -- bitwise `solve(A, b)`. lu_solve_t solves A^T x = b: with
    // P A = L U, A^T = U^T L^T P, so U^T y = b (forward, divide by the
    // diagonal), L^T w = y (backward, unit diagonal), x = P^T w (the swaps
    // undone in REVERSE step order). Interpreter twin: `luSolveArray`.
    // Routed arm: `?getrs` with 'N' / 'T' through `blade_lapack::blade_lu_solve_d`.
    let lName = exprToCppCore subst names lExpr
    let pName = exprToCppCore subst names pExpr
    let bName = exprToCppCore subst names rExpr
    (match inferExprType lExpr, inferExprType rExpr with
     | ArrayElem la, ArrayElem _ ->
        let outElemStr = irTypeToCpp la.ElemType
        let nExtent = $"{lName}.extents[0]"
        let extentsName = $"{varName}_extents"
        let nName = $"{varName}__n"
        let infoName = $"{varName}__info"
        let call = Blade.LinAlgPatterns.classifyLuSolve la
        let shimEntry = call |> Option.bind (Blade.LinAlgPatterns.shimEntryPoint Blade.LinAlgPatterns.HostBlas)
        match shimEntry with
        | Some _ -> (lapackUsedCell ()).Value <- true
        | None -> ()
        let (extentDecl, ownedExtents) = emitExtentsTable "" extentsName 1 [(nExtent, false)]
        let allocDecl =
            $"Array<{outElemStr}, 1> {varName} = {{ allocate<typename promote<{outElemStr}, 1>::type, nullptr>({extentsName}), {extentsName} }};"
        let modeWord = if transposed then "transposed" else "plain"
        let transArg = if transposed then 1 else 0
        let body =
            match shimEntry with
            | Some entry ->
                [ $"const size_t {nName} = {nExtent};"
                  $"/* lapack dispatch: lu_solve(LU, piv, b) -> x, {modeWord} */ int {infoName} = {entry}({nName}, {lName}.data, {pName}.data, {bName}.data, {varName}.data, {transArg});"
                  $$"""if ({{infoName}} != 0) { blade_rt::panic("BL8007", "lu_solve: the stored factorization is invalid", nullptr, 0); }""" ]
            | None when not transposed ->
                [ "/* lu solve: pivots, then L (unit lower), then U */"
                  $"const size_t {nName} = {nExtent};"
                  $$"""for (size_t __si = 0; __si < {{nName}}; __si++) { {{varName}}[__si] = {{bName}}[__si]; }"""
                  $$"""for (size_t __sk = 0; __sk < {{nName}}; __sk++) {"""
                  $"    size_t __sp = (size_t){pName}[__sk];"
                  $$"""    if (__sp != __sk) { {{outElemStr}} __sxt = {{varName}}[__sk]; {{varName}}[__sk] = {{varName}}[__sp]; {{varName}}[__sp] = __sxt; }"""
                  $$"""    for (size_t __si = __sk + 1; __si < {{nName}}; __si++) { {{varName}}[__si] = {{varName}}[__si] - {{lName}}[__si][__sk] * {{varName}}[__sk]; }"""
                  "}"
                  $$"""for (size_t __skk = {{nName}}; __skk > 0; __skk--) {"""
                  "    size_t __sk = __skk - 1;"
                  $"    {outElemStr} __ss = {varName}[__sk];"
                  $$"""    for (size_t __sj = __sk + 1; __sj < {{nName}}; __sj++) { __ss = __ss - {{lName}}[__sk][__sj] * {{varName}}[__sj]; }"""
                  $"    {varName}[__sk] = __ss / {lName}[__sk][__sk];"
                  "}" ]
            | None ->
                [ "/* lu solve transposed: U^T (forward), L^T (backward), then the pivots undone */"
                  $"const size_t {nName} = {nExtent};"
                  $$"""for (size_t __sk = 0; __sk < {{nName}}; __sk++) {"""
                  $"    {outElemStr} __ss = {bName}[__sk];"
                  $$"""    for (size_t __sj = 0; __sj < __sk; __sj++) { __ss = __ss - {{lName}}[__sj][__sk] * {{varName}}[__sj]; }"""
                  $"    {varName}[__sk] = __ss / {lName}[__sk][__sk];"
                  "}"
                  $$"""for (size_t __skk = {{nName}}; __skk > 0; __skk--) {"""
                  "    size_t __sk = __skk - 1;"
                  $"    {outElemStr} __ss = {varName}[__sk];"
                  $$"""    for (size_t __sj = __sk + 1; __sj < {{nName}}; __sj++) { __ss = __ss - {{lName}}[__sj][__sk] * {{varName}}[__sj]; }"""
                  $"    {varName}[__sk] = __ss;"
                  "}"
                  $$"""for (size_t __skk = {{nName}}; __skk > 0; __skk--) {"""
                  "    size_t __sk = __skk - 1;"
                  $"    size_t __sp = (size_t){pName}[__sk];"
                  $$"""    if (__sp != __sk) { {{outElemStr}} __sxt = {{varName}}[__sk]; {{varName}}[__sk] = {{varName}}[__sp]; {{varName}}[__sp] = __sxt; }"""
                  "}" ]
        Some (extentDecl @ [allocDecl; "{"] @ body @ ["}"],
              [MatPool (varName, outElemStr, 1, "nullptr", None, ownedExtents)])
     | _ -> None)

and materializeSolveForm (subst: SubstMap) (names: Map<IRId, string>) (varName: string) (elemTypeStr: string) (mExpr: IRExpr) (rExpr: IRExpr) : (string list * MaterializedAlloc list) option =
    // solve(A, b) -> x with A.x = b, by partial-pivoted LU. A : n x n dense,
    // b : n dense, x : a fresh dense rank-1 pool of n cells.
    //
    // TWO ARMS, AND BOTH ARE REAL -- the structural difference from
    // `materializeEighForm`, where the un-dispatched case is a `#error` because
    // the native math lives in source the elaborator declined to synthesize.
    // Here the native math is emitted RIGHT HERE, so `shimEntry = None` is the
    // ordinary path (and the default one, the gate being off by default).
    //
    // BYTE-IDENTITY, and what it is a claim about. The native arm below is the
    // operation-for-operation twin of `Interp/ArrayOps.solveArray`: same
    // row-major working copy, same pivot rule, same swap, same in-elimination
    // update of the right-hand side, same descending back-substitution, and
    // every arithmetic step written as an explicit `a - b * c` rather than a
    // compound assignment, so no step can be reassociated by rewriting. Those
    // two are what `blade test interp math` byte-compares. THE LAPACK ARM IS
    // NOT PART OF THAT CLAIM: `?gesv` is blocked and applies its pivots in a
    // different order, so it agrees to ~1e-14 and not to the ULP -- the same
    // standing policy every BLAS route has, and the reason the differential
    // harnesses run gate-off.
    //
    // THE PIVOT RULE, pinned in words because it is the one place two correct
    // implementations can silently disagree: scan column k from row k
    // downward, keep the FIRST row attaining the maximum |value| -- a STRICT
    // `>` comparison, so a later equal magnitude never displaces an earlier
    // one. LAPACK's `idamax` uses the same strict-greater tie-break, which is
    // why the two arms agree on WHICH matrix they factorize even though they
    // do not agree bitwise on the result.
    //
    // SINGULARITY is an EXACT `== 0.0` test on the chosen pivot, never an
    // epsilon. An epsilon would be a second tunable that the interpreter and
    // the C++ arm would have to keep numerically identical across a
    // difference-of-two-roundings, which is exactly the class of disagreement
    // this design removes. Exact zero is decidable identically everywhere, and
    // a nearly-singular matrix is the user's numerical problem, not a
    // compile-time one.
    let aName = exprToCppCore subst names mExpr
    let bName = exprToCppCore subst names rExpr
    let aTy = inferExprType mExpr
    let bTy = inferExprType rExpr
    (match aTy, bTy with
     | ArrayElem aa, ArrayElem ba ->
        let outElemStr = irTypeToCpp aa.ElemType
        let nExtent = $"{aName}.extents[0]"
        let extentsName = $"{varName}_extents"
        // Derived names, the `materializeEighForm` convention: everything this
        // form introduces at statement level is prefixed by the binding, so two
        // solves in one scope cannot collide. The loop-body temporaries below
        // need no prefix -- each is scoped to its own `for` body.
        let nName = $"{varName}__n"
        let luName = $"{varName}__lu"
        let infoName = $"{varName}__info"
        let call = Blade.LinAlgPatterns.classifySolve aa ba
        // HostBlas asked DIRECTLY, not through `resolveNodeRoute`: the CudaBlas
        // policy row for Solve is `Native` (cuSOLVER is a separate library), so
        // the chain would resolve to HostBlas anyway -- the same shortcut
        // `materializeEighForm` takes, and for the same recorded reason.
        let shimEntry = call |> Option.bind (Blade.LinAlgPatterns.shimEntryPoint Blade.LinAlgPatterns.HostBlas)
        match shimEntry with
        | Some _ -> (lapackUsedCell ()).Value <- true
        | None -> ()
        // Shared companion-extents rule: `n` is a runtime read, so this is the
        // heap arm, and `solve`'s answer survives leaving the frame.
        let (extentDecl, ownedExtents) =
            emitExtentsTable "" extentsName 1 [(nExtent, false)]
        let allocDecl =
            $"Array<{outElemStr}, 1> {varName} = {{ allocate<typename promote<{outElemStr}, 1>::type, nullptr>({extentsName}), {extentsName} }};"
        // BLOCK comments only, never `//`: these lines are SPACE-JOINED into a
        // single-line IIFE at expression positions, where a line comment would
        // swallow the rest of the statement (the lesson math/057 taught the
        // gram emitter).
        let panicLine (ind: string) =
            $"{ind}blade_rt::panic(\"BL8007\", \"{solveSingularMessage}\", nullptr, 0);"
        let luCell (i: string) (j: string) = $"{luName}[{i} * {nName} + {j}]"
        let body =
            match shimEntry with
            | Some entry ->
                // A is handed over as its ROW SKELETON and b as its flat pool,
                // exactly the subscripts Blade's own loops use, so a staged or
                // sliced operand contributes the identical values. The shim
                // owns the column-major bridge and the destroy-on-exit copies;
                // `info` comes back for THIS site to judge, keeping the panic
                // message in one place shared with the native arm rather than
                // duplicating it inside a header that has no runtime include.
                [ $"const size_t {nName} = {nExtent};"
                  $"/* lapack dispatch: solve(A, b) -> x, dense square operand, single right-hand side */ int {infoName} = {entry}({nName}, {aName}.data, {bName}.data, {varName}.data);"
                  $$"""if ({{infoName}} != 0) { {{(panicLine "")}} }""" ]
            | None ->
                [ $"const size_t {nName} = {nExtent};"
                  // The working copy. LU overwrites it, so A itself is never
                  // touched -- `solve(A, b)` twice over the same A is the same
                  // answer twice, which a factor-in-place would quietly break.
                  //
                  // SMALL SYSTEMS DO NOT TOUCH THE HEAP. The allocation, not the
                  // arithmetic, is what a small solve costs: measured on the
                  // prototype (src/microkernels/small_solve.c) the malloc/free
                  // pair is a FLAT 56-59 ns at every n, which is 73-85% of the
                  // entire gap between this emission and a fully specialized
                  // fixed-size kernel -- 13.3x of the 13.3x at n=2, and still
                  // three quarters of it at n=8. A 8x8 stack buffer captures
                  // that without any new codegen machinery, without specializing
                  // on n, and WITHOUT CHANGING THE ARITHMETIC: the same
                  // operations in the same order on the same storage layout, so
                  // this is bitwise identical (955/955 in the prototype) and
                  // needs no licence. Above the threshold the vector path is
                  // unchanged; `resize` on a default-constructed vector is the
                  // only allocation, and it never runs for small n.
                  $"{outElemStr} {luName}__stk[64];"
                  $"std::vector<{outElemStr}> {luName}__heap;"
                  $"{outElemStr}* {luName};"
                  $"if ({nName} <= 8) {{ {luName} = {luName}__stk; }} else {{ {luName}__heap.resize({nName} * {nName}); {luName} = {luName}__heap.data(); }}"
                  $$"""for (size_t __si = 0; __si < {{nName}}; __si++) {"""
                  $$"""    for (size_t __sj = 0; __sj < {{nName}}; __sj++) { {{(luCell "__si" "__sj")}} = {{aName}}[__si][__sj]; }"""
                  // x starts life as b and is transformed in place: the forward
                  // substitution is FUSED INTO the elimination (each multiplier
                  // is applied to the right-hand side the moment it is formed),
                  // so there is no separate L-solve pass and no permutation
                  // vector to replay. `solveArray` does exactly this.
                  $"    {varName}[__si] = {bName}[__si];"
                  "}"
                  $$"""for (size_t __sk = 0; __sk < {{nName}}; __sk++) {"""
                  "    size_t __sp = __sk;"
                  $"""    {outElemStr} __sbig = std::fabs({(luCell "__sk" "__sk")});"""
                  $$"""    for (size_t __si = __sk + 1; __si < {{nName}}; __si++) {"""
                  $"""        {outElemStr} __sm = std::fabs({(luCell "__si" "__sk")});"""
                  // STRICT `>`: first maximal magnitude wins. See the pivot-rule
                  // note above -- this single character is the tie-break.
                  "        if (__sm > __sbig) { __sbig = __sm; __sp = __si; }"
                  "    }"
                  $$"""    if ({{(luCell "__sp" "__sk")}} == {{outElemStr}}(0)) { {{(panicLine "")}} }"""
                  "    if (__sp != __sk) {"
                  $$"""        for (size_t __sj = 0; __sj < {{nName}}; __sj++) {"""
                  $"""            {outElemStr} __st = {(luCell "__sk" "__sj")};"""
                  $"""            {(luCell "__sk" "__sj")} = {(luCell "__sp" "__sj")};"""
                  $"""            {(luCell "__sp" "__sj")} = __st;"""
                  "        }"
                  $"        {outElemStr} __sxt = {varName}[__sk]; {varName}[__sk] = {varName}[__sp]; {varName}[__sp] = __sxt;"
                  "    }"
                  $$"""    for (size_t __si = __sk + 1; __si < {{nName}}; __si++) {"""
                  $"""        {outElemStr} __sf = {(luCell "__si" "__sk")} / {(luCell "__sk" "__sk")};"""
                  $"""        {(luCell "__si" "__sk")} = __sf;"""
                  $$"""        for (size_t __sj = __sk + 1; __sj < {{nName}}; __sj++) {"""
                  // Written `a = a - f * b`, never `a -= f * b`: identical in
                  // C++, but the explicit form is the one the interpreter twin
                  // reads as a single subtract-of-a-product, so the two texts
                  // can be diffed against each other by eye.
                  $"""            {(luCell "__si" "__sj")} = {(luCell "__si" "__sj")} - __sf * {(luCell "__sk" "__sj")};"""
                  "        }"
                  $"        {varName}[__si] = {varName}[__si] - __sf * {varName}[__sk];"
                  "    }"
                  "}"
                  // Back substitution, k descending. Counted DOWN through an
                  // unsigned `__skk` from n to 1 rather than `for (size_t k = n
                  // - 1; k >= 0; k--)`, which never terminates on an unsigned
                  // index -- and would read as correct.
                  $$"""for (size_t __skk = {{nName}}; __skk > 0; __skk--) {"""
                  "    size_t __sk = __skk - 1;"
                  $"    {outElemStr} __ss = {varName}[__sk];"
                  $$"""    for (size_t __sj = __sk + 1; __sj < {{nName}}; __sj++) { __ss = __ss - {{(luCell "__sk" "__sj")}} * {{varName}}[__sj]; }"""
                  $"""    {varName}[__sk] = __ss / {(luCell "__sk" "__sk")};"""
                  "}" ]
        // Everything the form introduces beyond `varName` itself is wrapped in
        // one block so a second solve in the same scope re-declares nothing.
        // `varName` and its extents table stay OUTSIDE it -- they are the value.
        Some (extentDecl @ [allocDecl; "{"] @ body @ ["}"],
              [MatPool (varName, outElemStr, 1, "nullptr", None, ownedExtents)])
     | _ -> None)

