module Blade.Tests.ExprAttrs

// ============================================================================
// Phase B corpus tests for ExprAttrs.
//
// Each test directly constructs an IR fragment with known FreeVars and
// BoundVars, calls exprAttrs, and reports whether the actual result matches
// the expected one. The tests do NOT go through parse/typecheck/lower;
// they are unit tests on the attribute computation only.
//
// The purpose is to validate that exprAttrs correctly handles:
//   - leaves (constants, variables, params)
//   - binders (IRLet, IRLambda, IRForRange, IRMatch with patterns)
//   - capture semantics (lambda body refs to outer-scope IRIds)
//   - shadowing (inner binder of an id that's also free outside)
//   - every inline-form arm and IR construct, to catch arms a future
//     code-change might miss when adding a new variant
// ============================================================================

open Blade.IR
open Blade.IRLoopStructure
open Blade.IRStorage
open Blade.IRLift
open Blade.IRMono
open Blade.IRPrint
open Blade.IRValidate
open Blade.Types
open Blade.Tests.TestHarness

/// A single corpus test: name + a thunk that returns the actual attrs and
/// the expected attrs. The runner compares them and reports per-test.
type AttrsTest = {
    Name: string
    Run: unit -> ExprAttrs * ExprAttrs   // (actual, expected)
}

let private mkAttrs (free: int list) (bound: int list) (isPure: bool) : ExprAttrs =
    { FreeVars  = Set.ofList free
      BoundVars = Set.ofList bound
      IsPure    = isPure }

let private intTy = IRTScalar ETInt64
let private boolTy = IRTScalar ETBool

/// ------------------------------------------------------------
/// 1. Leaves
/// ------------------------------------------------------------

let test_lit_has_no_refs = {
    Name = "Leaf: literal has empty attrs"
    Run = fun () ->
        let actual = exprAttrs (IRLit (IRLitInt 42L))
        let expected = mkAttrs [] [] true
        (actual, expected)
}

let test_var_is_free = {
    Name = "Leaf: IRVar contributes one FreeVar"
    Run = fun () ->
        let actual = exprAttrs (IRVar (7, intTy))
        let expected = mkAttrs [7] [] true
        (actual, expected)
}

let test_param_has_no_refs = {
    Name = "Leaf: IRParam has empty attrs (params are not variables)"
    Run = fun () ->
        let actual = exprAttrs (IRParam ("p", 0, intTy))
        let expected = mkAttrs [] [] true
        (actual, expected)
}

/// ------------------------------------------------------------
/// 2. Simple compositions
/// ------------------------------------------------------------

let test_binop_unions_children = {
    Name = "BinOp: free vars from both operands union"
    Run = fun () ->
        // x + y → free {x, y}
        let e = IRBinOp (IRElementwise, IRAdd, IRVar (1, intTy), IRVar (2, intTy))
        let actual = exprAttrs e
        let expected = mkAttrs [1; 2] [] true
        (actual, expected)
}

let test_unaryop_passes_through = {
    Name = "UnaryOp: free vars pass through"
    Run = fun () ->
        // -x → free {x}
        let e = IRUnaryOp (IRNeg, IRVar (3, intTy))
        let actual = exprAttrs e
        let expected = mkAttrs [3] [] true
        (actual, expected)
}

let test_tuple_unions_all = {
    Name = "Tuple: free vars from all elements union"
    Run = fun () ->
        // (x, y, x) → free {x, y}
        let e = IRTuple [IRVar (1, intTy); IRVar (2, intTy); IRVar (1, intTy)]
        let actual = exprAttrs e
        let expected = mkAttrs [1; 2] [] true
        (actual, expected)
}

/// ------------------------------------------------------------
/// 3. IRLet binder
/// ------------------------------------------------------------

let test_let_binds_id = {
    Name = "Let: binder ID is added to BoundVars, removed from FreeVars"
    Run = fun () ->
        // let x = 1 in x + y
        // FreeVars: {y}; BoundVars: {x}
        let e =
            IRLet (10, IRLit (IRLitInt 1L),
                IRBinOp (IRElementwise, IRAdd, IRVar (10, intTy), IRVar (20, intTy)))
        let actual = exprAttrs e
        let expected = mkAttrs [20] [10] true
        (actual, expected)
}

let test_let_shadowing = {
    Name = "Let: inner let shadows outer free"
    Run = fun () ->
        // y is free; in body, let y = ... rebinds it; the binding shows up
        // in BoundVars, but FreeVars of the inner subexpression that uses
        // the inner y won't surface y as free. The OUTER let binds id 10,
        // and the inner let binds id 20. Both should appear in BoundVars.
        // FreeVars = {z} (referenced after both binders).
        //
        //   let x10 = z in
        //     let y20 = 5 in
        //       x10 + y20
        let inner = IRLet (20, IRLit (IRLitInt 5L),
                        IRBinOp (IRElementwise, IRAdd, IRVar (10, intTy), IRVar (20, intTy)))
        let e = IRLet (10, IRVar (99, intTy), inner)
        let actual = exprAttrs e
        let expected = mkAttrs [99] [10; 20] true
        (actual, expected)
}

let test_let_value_can_be_free = {
    Name = "Let: value's free vars are not bound by the let's own ID"
    Run = fun () ->
        // let x = y in x + 1
        // FreeVars: {y}; BoundVars: {x}
        let e =
            IRLet (10, IRVar (5, intTy),
                IRBinOp (IRElementwise, IRAdd, IRVar (10, intTy), IRLit (IRLitInt 1L)))
        let actual = exprAttrs e
        let expected = mkAttrs [5] [10] true
        (actual, expected)
}

/// ------------------------------------------------------------
/// 4. IRLambda binder
/// ------------------------------------------------------------
///
/// Stage 3c.4c: the IRLambda variant has been retired from the IR
/// type. Lambdas in surface Blade lower to lifted IRCallables
/// referenced via IRVar(callable.Id) at use sites; their binder
/// semantics now live at the IRFuncDef level (which isn't an IRExpr
/// and so isn't testable through exprAttrs). The three IRLambda
/// binder tests that lived here (test_lambda_binds_params,
/// test_lambda_captures_outer, test_nested_lambda) are removed.

/// ------------------------------------------------------------
/// 5. IRForRange and IRMatch binders
/// ------------------------------------------------------------

let test_forrange_binds_loop_var = {
    Name = "ForRange: loop varId is bound; lo/hi can have free vars"
    Run = fun () ->
        // for i:30 in 0..n { i + acc }
        // FreeVars: {n, acc}; BoundVars: {i (30)}
        let body = IRBinOp (IRElementwise, IRAdd, IRVar (30, intTy), IRVar (5, intTy))
        let e = IRForRange (30, IRLit (IRLitInt 0L), IRVar (99, intTy), body)
        let actual = exprAttrs e
        let expected = mkAttrs [99; 5] [30] true
        (actual, expected)
}

let test_match_pattern_binds = {
    Name = "Match: pattern var IDs are bound in case body"
    Run = fun () ->
        // match scrut(99) with
        //   | x_5 -> x_5 + y_8     // pattern binds 5; body refs 5 and 8
        let case: IRMatchCase = {
            Pattern = IRPatVar 5
            Guard = None
            Body = IRBinOp (IRElementwise, IRAdd, IRVar (5, intTy), IRVar (8, intTy))
        }
        let e = IRMatch (IRVar (99, intTy), [case])
        let actual = exprAttrs e
        // scrut contributes 99 free; case body has 5 bound, 8 free
        let expected = mkAttrs [99; 8] [5] true
        (actual, expected)
}

let test_match_tuple_pattern = {
    Name = "Match: tuple pattern binds multiple IDs"
    Run = fun () ->
        // match scrut(99) with
        //   | (a_5, b_6) -> a_5 + b_6 + c_8
        let case: IRMatchCase = {
            Pattern = IRPatTuple [IRPatVar 5; IRPatVar 6]
            Guard = None
            Body =
                IRBinOp (IRElementwise, IRAdd,
                    IRBinOp (IRElementwise, IRAdd, IRVar (5, intTy), IRVar (6, intTy)),
                    IRVar (8, intTy))
        }
        let e = IRMatch (IRVar (99, intTy), [case])
        let actual = exprAttrs e
        let expected = mkAttrs [99; 8] [5; 6] true
        (actual, expected)
}

/// ------------------------------------------------------------
/// 6. Every inline-form arm — guards against future arm omissions
/// ------------------------------------------------------------

let test_mask_arm = {
    Name = "Inline: IRMask unions array and predicate"
    Run = fun () ->
        // mask(a_1, p_2)
        let e = IRMask (IRVar (1, intTy), IRVar (2, intTy))
        let actual = exprAttrs e
        let expected = mkAttrs [1; 2] [] true
        (actual, expected)
}

let test_intersect_arm = {
    Name = "Inline: IRIntersect unions both arrays"
    Run = fun () ->
        let e = IRIntersect (IRVar (1, intTy), IRVar (2, intTy))
        let actual = exprAttrs e
        let expected = mkAttrs [1; 2] [] true
        (actual, expected)
}

let test_union_arm = {
    Name = "Inline: IRUnion unions both arrays"
    Run = fun () ->
        let e = IRUnion (IRVar (1, intTy), IRVar (2, intTy))
        let actual = exprAttrs e
        let expected = mkAttrs [1; 2] [] true
        (actual, expected)
}

let test_unique_arm = {
    Name = "Inline: IRUnique has the input array's free vars"
    Run = fun () ->
        let e = IRUnique (IRVar (1, intTy))
        let actual = exprAttrs e
        let expected = mkAttrs [1] [] true
        (actual, expected)
}

let test_contains_arm = {
    Name = "Inline: IRContains contributes a probe and unions array + value"
    Run = fun () ->
        // contains(a_1, v_2): probe carries BuildOn = a_1; the value
        // child is recursed into for FreeVars.
        let arr = IRVar (1, intTy)
        let e = IRContains (arr, IRVar (2, intTy))
        let actual = exprAttrs e
        let expected = mkAttrs [1; 2] [] true
        (actual, expected)
}

let test_sort_arm = {
    Name = "Inline: IRSort unions array and key"
    Run = fun () ->
        let e = IRSort (IRVar (1, intTy), IRVar (2, intTy))
        let actual = exprAttrs e
        let expected = mkAttrs [1; 2] [] true
        (actual, expected)
}

let test_reduce_arm = {
    Name = "Inline: IRReduce unions array and kernel"
    Run = fun () ->
        let e = IRReduce (IRVar (1, intTy), IRVar (2, intTy), None)
        let actual = exprAttrs e
        let expected = mkAttrs [1; 2] [] true
        (actual, expected)
}

/// ------------------------------------------------------------
/// 7. Constructs that previously fell through collectVarRefsIR's
///    catch-all — verify exhaustive handling
/// ------------------------------------------------------------

let test_slice_finds_refs = {
    Name = "Exhaustiveness: IRSlice picks up start/stop refs"
    Run = fun () ->
        // arr_1[2:start_3:stop_4]
        let e = IRSlice (IRVar (1, intTy), 0, IRVar (3, intTy), IRVar (4, intTy))
        let actual = exprAttrs e
        let expected = mkAttrs [1; 3; 4] [] true
        (actual, expected)
}

let test_join_finds_refs = {
    Name = "Exhaustiveness: IRJoin picks up refs in joined arrays"
    Run = fun () ->
        let e = IRJoin ([IRVar (1, intTy); IRVar (2, intTy); IRVar (3, intTy)], 0)
        let actual = exprAttrs e
        let expected = mkAttrs [1; 2; 3] [] true
        (actual, expected)
}

let test_shift_with_pad = {
    Name = "Exhaustiveness: IRShift's BndPad expression is scanned"
    Run = fun () ->
        // shift(arr_1, dim, offset_2, BndPad fill_3)
        let e = IRShift (IRVar (1, intTy), 0, IRVar (2, intTy), BndPad (IRVar (3, intTy)))
        let actual = exprAttrs e
        let expected = mkAttrs [1; 2; 3] [] true
        (actual, expected)
}

/// ------------------------------------------------------------
/// 8. The key composition: nested binder + outer free var
/// ------------------------------------------------------------
///
/// Stage 3c.4c: the test that lived here
/// (test_mask_lambda_binder_does_not_leak) constructed an
/// IRLambda predicate inline. With IRLambda retired from the IR
/// type, predicates arrive as IRVar references; the
/// "lambda-bound param doesn't leak" semantic is now trivially
/// preserved because exprAttrs over an IRVar doesn't see the
/// callable's body in the first place. The actual probe
/// optimization (IRMask → IRMaskWithSet) is tested via
/// tryRewriteMaskContains end-to-end through the full pipeline,
/// not via exprAttrs.

/// ------------------------------------------------------------
/// 9. Contains probe propagation
/// ------------------------------------------------------------
/// These tests validate the Phase C foundation: that any IRContains
/// node anywhere inside an expression flows up as a ContainsProbe
/// through every compositional construct, and that IRMask is the only
/// node that consumes them.

let test_probe_in_binop = {
    Name = "Probe: contains inside a binop propagates upward"
    Run = fun () ->
        // contains(B_1, x_2) && true_lit
        // → probe with BuildOn = B_1 reaches the top
        let arrB = IRVar (1, intTy)
        let cont = IRContains (arrB, IRVar (2, intTy))
        let e = IRBinOp (IRElementwise, IRAnd, cont, IRLit (IRLitBool true))
        let actual = exprAttrs e
        let expected = mkAttrs [1; 2] [] true
        (actual, expected)
}

let test_probe_in_if = {
    Name = "Probe: contains inside an if-branch propagates upward"
    Run = fun () ->
        // if cond_1 then contains(B_2, x_3) else false
        let arrB = IRVar (2, intTy)
        let cont = IRContains (arrB, IRVar (3, intTy))
        let e = IRIf (IRVar (1, boolTy), cont, IRLit (IRLitBool false))
        let actual = exprAttrs e
        let expected = mkAttrs [1; 2; 3] [] true
        (actual, expected)
}

// Stage 3c.4c: four tests that lived in this section have been removed
// (test_probe_through_nested_lambda, test_mask_consumes_probe,
// test_mask_consumes_only_predicate_probes, test_nested_mask_no_outer_probe).
// All four constructed IRLambda predicates inline. With IRLambda retired,
// predicates arrive as IRVar references whose callable bodies aren't
// walked by exprAttrs over IRMask. The probe-consume semantic is now
// trivially preserved (the predicate slot contributes no probes to
// begin with), and the actual cross-procedural probe analysis that
// enables IRMask → IRMaskWithSet rewriting is driven by
// tryRewriteMaskContains using resolveCallable directly on the
// callable body — verified end-to-end via the full-pipeline mask tests.

let test_multiple_probes_in_one_predicate = {
    Name = "Probe: two distinct contains calls in a predicate both collected"
    Run = fun () ->
        // contains(B_1, x_3) || contains(C_2, x_3)
        // Two probes, BuildOn = B_1 and C_2 respectively, in source order.
        let arrB = IRVar (1, intTy)
        let arrC = IRVar (2, intTy)
        let cont1 = IRContains (arrB, IRVar (3, intTy))
        let cont2 = IRContains (arrC, IRVar (3, intTy))
        let e = IRBinOp (IRElementwise, IROr, cont1, cont2)
        let actual = exprAttrs e
        let expected = mkAttrs [1; 2; 3] [] true
        (actual, expected)
}

let test_probe_in_let_value = {
    Name = "Probe: contains inside a let-value propagates upward"
    Run = fun () ->
        // let z_10 = contains(B_1, x_2) in z_10
        // The probe in the let-value flows up through the let.
        let arrB = IRVar (1, intTy)
        let cont = IRContains (arrB, IRVar (2, intTy))
        let e = IRLet (10, cont, IRVar (10, boolTy))
        let actual = exprAttrs e
        // FreeVars: {1, 2}; BoundVars: {10}; probe propagates.
        let expected = mkAttrs [1; 2] [10] true
        (actual, expected)
}

/// The callee shared by the two cross-procedural tests below:
///
///   f(arr, x) = contains(arr, x) + g_55
///
/// The `+ IRVar(55)` matters. VarId 55 is NOT one of f's parameters, so the
/// substitution map (arr→actual, x→actual) leaves it alone and it reaches the
/// caller's FreeVars *only* if the walker actually descended into the body.
/// A body made purely of parameter references cannot distinguish "walked and
/// substituted" from "never walked": both yield exactly the args' own free
/// vars, which the IRApp arm's baseAttrs already contributes. (That was the
/// old shape of this test — it asserted the walker's output to be identical
/// to no-walker-at-all.)
let private crossProcCallee : IRCallable =
    let arrP : IRParam = { Name = "arr"; Type = intTy; Index = 0; VarId = 77 }
    let xP   : IRParam = { Name = "x";   Type = intTy; Index = 1; VarId = 78 }
    let fBody =
        IRBinOp (IRElementwise, IRAdd,
            IRContains (IRVar (77, intTy), IRVar (78, intTy)),
            IRVar (55, intTy))
    { Id = 99; Name = "f"
      Params = [arrP; xP]; RetType = boolTy; Body = fBody
      IsStatic = false
      IsCommutative = false; CommGroups = []; AntisymGroups = []
      Parallelism = []; IsOmpParallel = false; IsCudaKernel = false; CudaBlockSize = 256; IsMpiParallel = false; IsArityPoly = false; ArityParam = None
      IsRepro = false
      Captures = []; SignParities = []
      Effects = Blade.Effects.unknown }

/// The call site both tests evaluate: `f(B_1, x_2)`.
let private crossProcCallSite : IRExpr =
    IRApp (IRVar (99, intTy), [IRVar (1, intTy); IRVar (2, intTy)], intTy)

let test_probe_imported_via_callable_table = {
    Name = "Probe: cross-procedural propagation via unified callable walker"
    Run = fun () ->
        // A callable `f(arr, x) = contains(arr, x) + g_55` registered in the
        // CallablesTable; the caller expression is `f(B_1, x_2)`. The walker
        // descends into f's body with arr→B_1, x→x_2 applied, sees the
        // IRContains, and surfaces a probe whose BuildOn is IRVar(B_1).
        let callables : CallablesTable = Map.ofList [(99, crossProcCallee)]
        let prev = setCallablesContext callables
        try
            let actual = exprAttrs crossProcCallSite
            // FreeVars at the IRApp arm (baseAttrs): {99 (the function ref),
            // 1, 2}. From the walked body: IRVar(77) is substituted to
            // IRVar(1) and IRVar(78) to IRVar(2) (contributing nothing new,
            // which is the substitution working), and the NON-parameter
            // IRVar(55) survives — so the walk adds exactly {55}.
            // Probes: one probe with BuildOn = IRVar(1) (after substitution).
            let expected = mkAttrs [99; 1; 2; 55] [] true
            (actual, expected)
        finally
            restoreAnalysisContext prev
}

let test_no_probe_import_without_callable_table = {
    Name = "Probe: same call site under an EMPTY callables table does NOT import the body"
    Run = fun () ->
        // The paired negative of the test above, over the identical IRApp.
        // With nothing resolvable in the table the IRApp arm returns
        // baseAttrs alone, so the callee's non-parameter free var 55 must be
        // ABSENT. Together the pair pins both halves of the walker: that it
        // walks (55 appears above) and that it substitutes (77/78 never
        // appear, in either direction).
        let prev = setCallablesContext Map.empty
        try
            let actual = exprAttrs crossProcCallSite
            let expected = mkAttrs [99; 1; 2] [] true
            (actual, expected)
        finally
            restoreAnalysisContext prev
}


let allAttrsTests : AttrsTest list = [
    test_lit_has_no_refs
    test_var_is_free
    test_param_has_no_refs
    test_binop_unions_children
    test_unaryop_passes_through
    test_tuple_unions_all
    test_let_binds_id
    test_let_shadowing
    test_let_value_can_be_free
    test_forrange_binds_loop_var
    test_match_pattern_binds
    test_match_tuple_pattern
    test_mask_arm
    test_intersect_arm
    test_union_arm
    test_unique_arm
    test_contains_arm
    test_sort_arm
    test_reduce_arm
    test_slice_finds_refs
    test_join_finds_refs
    test_shift_with_pad
    // Phase C foundation: contains-probe propagation
    test_probe_in_binop
    test_probe_in_if
    test_multiple_probes_in_one_predicate
    test_probe_in_let_value
    // Phase C unified walker: cross-procedural propagation via CallablesTable.
    // The pair is load-bearing — see crossProcCallee's comment.
    test_probe_imported_via_callable_table
    test_no_probe_import_without_callable_table
]

/// Pretty-print an ExprAttrs for diffing. The sets are printed sorted so
/// the output is stable regardless of insertion order. Probes are listed
/// in collection order with just their BuildOn shown (Node is reference-
/// equality data, not human-readable).
let private fmtAttrs (a: ExprAttrs) : string =
    let setStr (s: Set<IRId>) =
        s |> Set.toList |> List.sort
          |> List.map string |> String.concat ", "
    sprintf "{ Free={%s}; Bound={%s}; Pure=%b }"
        (setStr a.FreeVars) (setStr a.BoundVars) a.IsPure

let private attrsEqual (a: ExprAttrs) (b: ExprAttrs) : bool =
    a.FreeVars = b.FreeVars
        && a.BoundVars = b.BoundVars
        && a.IsPure = b.IsPure

/// Run all attribute tests, returning (passed, failed). Prints per-test
/// status; on failure also prints actual vs expected so the difference
/// is visible without re-running with extra flags.
let runAttrsTests () : Blade.Tests.TestHarness.BlockResult =
    Blade.Tests.TestHarness.printHeader "ExprAttrs Corpus"
    let mutable passed = 0
    let mutable failed = 0
    let mutable failedNames = []
    for t in allAttrsTests do
        let (actual, expected) = t.Run ()
        if attrsEqual actual expected then
            passed <- passed + 1
            Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Pass t.Name ""
        else
            failed <- failed + 1
            failedNames <- failedNames @ [t.Name]
            let detail = $"expected {(fmtAttrs expected)}, got {(fmtAttrs actual)}"
            Blade.Tests.TestHarness.resultLine Blade.Tests.TestHarness.Fail t.Name detail
    Blade.Tests.TestHarness.printFooter "ExprAttrs" [$"{passed} passed"; $"{failed} failed"]
    { Block = "ExprAttrs"; Passed = passed; Failed = failed; Skipped = 0; FailedNames = failedNames }

