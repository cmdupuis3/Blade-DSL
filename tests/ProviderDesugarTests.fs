// Unit pins for src/ProviderDesugar.fs -- the icechunk `checkout` desugar.
//
// Fully hermetic and in-process: the pass is a raw-AST -> raw-AST rewrite, so
// every case here is parse -> expand -> assert on the resulting nodes. No
// store, no provider registry, no g++, and deliberately no typecheck --
// `import icechunk` does not resolve until the provider itself lands
// (docs/plans/plan-icechunk-provider.md, P2), and the desugar is specified to
// run BEFORE any of that.
//
// What is pinned: both accepted call shapes, the canonical key they build,
// SPAN FIDELITY on every synthesized node (the plan's section 13 makes this
// part of the gate -- diagnostics must point at the checkout text, not at the
// key), the four untouched cases, idempotence, and the loud refusal.
module Blade.Tests.ProviderDesugarTests

open Blade
open Blade.Ast
open Blade.Tests.TestHarness

/// Runs as a `BlockResult` so `tests/RunAll.fs` folds it into the grand
/// total: a raw-AST rewrite with no store, registry, or g++ behind it belongs
/// in the default suite, not only behind the standalone
/// `blade test provider-desugar` verb (the thin wrapper below).
let runProviderDesugarBlock () : BlockResult =
    printHeader "Provider Desugar (icechunk checkout)"
    let mutable passed = 0
    let mutable failed = 0
    let mutable failedNames : string list = []

    let check (name: string) (condition: bool) (detail: string) =
        if condition then
            printfn "  PASS: %s" name
            passed <- passed + 1
        else
            printfn "  FAIL: %s -- %s" name detail
            failed <- failed + 1
            failedNames <- name :: failedNames

    // ---------------------------------------------------------------
    // Fixture helpers
    // ---------------------------------------------------------------

    /// A parse failure in a fixture is a TEST bug, not a finding -- fail
    /// loudly rather than letting the case degrade into a vacuous pass.
    let parse (src: string) : Program =
        match Parser.parseProgram src with
        | Ok p -> p
        | Error e -> failwith $"test fixture does not parse ({e.Line}:{e.Col}): {e.Message}"

    /// The right-hand side of the top-level `let` / `let static` binding `name`.
    let declValue (p: Program) (name: string) : Expr option =
        p.Modules
        |> List.collect (_.Decls)
        |> List.tryPick (fun d ->
            match d.Value with
            | DeclLet b | DeclStatic b ->
                (match b.Pattern.Kind with
                 | PatternKind.PatVar n when n = name -> Some b.Value
                 | _ -> None)
            | _ -> None)

    let valueOf (p: Program) (name: string) : Expr =
        match declValue p name with
        | Some e -> e
        | None -> failwith $"test fixture has no top-level binding named {name}"

    /// (alias, path) when this expression is exactly `alias.load("path")`.
    let asLoad (e: Expr) : (string * string) option =
        match e.Kind with
        | ExprKind.ExprApp ({ Kind = ExprKind.ExprField ({ Kind = ExprKind.ExprVar a }, "load") },
                            [ { Kind = ExprKind.ExprLit (LitString path) } ]) -> Some (a, path)
        | _ -> None

    let loadOf (p: Program) (name: string) : (string * string) option =
        declValue p name |> Option.bind asLoad

    let isCheckout (e: Expr) : bool =
        match e.Kind with
        | ExprKind.ExprApp ({ Kind = ExprKind.ExprField (_, "checkout") }, _) -> true
        | _ -> false

    /// The four spans a `recv.member(arg0, ...)` call owns: the whole
    /// application, the `recv.member` field node, the receiver, and the first
    /// argument. The desugar copies all four onto its replacement, so ONE
    /// extractor serves both the before and the after shape.
    let callSpans (e: Expr) : (Span * Span * Span * Span) option =
        match e.Kind with
        | ExprKind.ExprApp (({ Kind = ExprKind.ExprField (recvE, _) } as headE), (arg0 :: _)) ->
            Some (e.Span, headE.Span, recvE.Span, arg0.Span)
        | _ -> None

    let spanStr (s: Span) = $"{s.StartLine}:{s.StartCol}-{s.EndLine}:{s.EndCol}"
    let shown (v: (string * string) option) = $"%A{v}"
    let msgs (ds: Blade.Diagnostics.Diagnostic list) =
        ds |> List.map (_.Message) |> String.concat "; "

    let repoSrc = """
import icechunk as ic

let repo = ic.load("data/weather.icechunk")
let ck1 = repo.checkout("main")
let ck2 = repo.checkout("v1.0", ic.tag)
let ck3 = repo.checkout("main", ic.branch)
let ck4 = repo.checkout("1CECHNKREP0F1RSTCMT0", ic.snapshot)
"""

    // ---------------------------------------------------------------
    // 1. The canonical key
    // ---------------------------------------------------------------
    printfn "\n--- canonical key ---"
    let markerKey = ProviderDesugar.canonicalKey "data/weather.icechunk" "tag" "v1.0"
    check "canonicalKey: marker form" (markerKey = "data/weather.icechunk@tag:v1.0") markerKey
    let bareKey = ProviderDesugar.canonicalKey "s" "?" "main"
    check "canonicalKey: bare form uses `?`" (bareKey = "s@?:main") bareKey

    // ---------------------------------------------------------------
    // 2. Bare and marker forms rewrite to the load shape
    // ---------------------------------------------------------------
    printfn "\n--- bare + marker rewrites ---"
    (match ProviderDesugar.expand (parse repoSrc) with
     | Error ds -> check "repo fixture desugars without diagnostics" false (msgs ds)
     | Ok p ->
         check "repo fixture desugars without diagnostics" true ""
         let handle = loadOf p "repo"
         check "the repo handle itself is left alone"
             (handle = Some ("ic", "data/weather.icechunk")) (shown handle)
         let l1 = loadOf p "ck1"
         check "bare checkout -> @?:main"
             (l1 = Some ("ic", "data/weather.icechunk@?:main")) (shown l1)
         let l2 = loadOf p "ck2"
         check "ic.tag -> @tag:v1.0"
             (l2 = Some ("ic", "data/weather.icechunk@tag:v1.0")) (shown l2)
         let l3 = loadOf p "ck3"
         check "ic.branch -> @branch:main"
             (l3 = Some ("ic", "data/weather.icechunk@branch:main")) (shown l3)
         let l4 = loadOf p "ck4"
         check "ic.snapshot -> @snapshot:<id>"
             (l4 = Some ("ic", "data/weather.icechunk@snapshot:1CECHNKREP0F1RSTCMT0")) (shown l4)
         check "no `checkout` node survives the rewrite"
             ([ "ck1"; "ck2"; "ck3"; "ck4" ]
              |> List.forall (fun n -> not (isCheckout (valueOf p n)))) "")

    // ---------------------------------------------------------------
    // 3. Span fidelity (plan section 13: diagnostics point at the checkout
    //    text, never at the synthesized key)
    // ---------------------------------------------------------------
    printfn "\n--- span fidelity ---"
    (let raw = parse repoSrc
     match callSpans (valueOf raw "ck2"), ProviderDesugar.expand raw with
     | Some (rawApp, rawHead, rawRecv, rawArg), Ok p ->
         // A test comparing two noSpans would pass vacuously.
         check "fixture spans are real (not noSpan)"
             (rawApp.StartLine > 0 && rawHead.StartLine > 0
              && rawRecv.StartLine > 0 && rawArg.StartLine > 0)
             (spanStr rawApp)
         (match callSpans (valueOf p "ck2") with
          | Some (newApp, newHead, newRecv, newArg) ->
              check "span: the load application keeps the checkout call's span"
                  (newApp = rawApp) $"{spanStr newApp} vs {spanStr rawApp}"
              check "span: `ic.load` keeps `repo.checkout`'s span"
                  (newHead = rawHead) $"{spanStr newHead} vs {spanStr rawHead}"
              check "span: the alias keeps the repo receiver's span"
                  (newRecv = rawRecv) $"{spanStr newRecv} vs {spanStr rawRecv}"
              check "span: the canonical key keeps the ref-name literal's span"
                  (newArg = rawArg) $"{spanStr newArg} vs {spanStr rawArg}"
          | None -> check "the rewritten ck2 is a call with spans" false "shape mismatch")
     | _, Error ds -> check "span fixture desugars" false (msgs ds)
     | None, _ -> check "the raw ck2 is a call with spans" false "shape mismatch")

    // ---------------------------------------------------------------
    // 4. `let static` bindings rewrite too -- StaticEval's providerRoots scan
    //    recognizes both, so the desugar has to serve both
    // ---------------------------------------------------------------
    printfn "\n--- static bindings ---"
    (let src = """
import icechunk as ic

let repo = ic.load("r")
let static ck = repo.checkout("main", ic.branch)
"""
     match ProviderDesugar.expand (parse src) with
     | Ok p ->
         let l = loadOf p "ck"
         check "let static checkout rewrites" (l = Some ("ic", "r@branch:main")) (shown l)
     | Error ds -> check "let static checkout rewrites" false (msgs ds))

    // ---------------------------------------------------------------
    // 5. A bare `import icechunk` (no alias) IS the alias
    // ---------------------------------------------------------------
    printfn "\n--- unaliased import ---"
    (let src = """
import icechunk

let repo = icechunk.load("r")
let ck = repo.checkout("v1", icechunk.tag)
"""
     match ProviderDesugar.expand (parse src) with
     | Ok p ->
         let l = loadOf p "ck"
         check "bare `import icechunk` binds the alias `icechunk`"
             (l = Some ("icechunk", "r@tag:v1")) (shown l)
     | Error ds -> check "bare `import icechunk` binds the alias `icechunk`" false (msgs ds))

    // ---------------------------------------------------------------
    // 6. The untouched cases
    // ---------------------------------------------------------------
    printfn "\n--- untouched receivers ---"

    // 6a. A receiver that is not a recorded repo handle: left alone, and NOT
    //     an error. It fails downstream as the ordinary missing-member error
    //     it is, which is the right message.
    (let src = """
import icechunk as ic

let repo = ic.load("r")
let bogus = 42
let ck = bogus.checkout("main")
"""
     let raw = parse src
     match ProviderDesugar.expand raw with
     | Ok p ->
         check "non-repo receiver: node untouched" (isCheckout (valueOf p "ck")) "rewritten anyway"
         check "non-repo receiver: program comes back reference-equal"
             (LanguagePrimitives.PhysicalEquality p raw) "a new Program was built"
     | Error ds -> check "non-repo receiver: no diagnostic" false (msgs ds))

    // 6b. No icechunk import at all -- the fast path. A `.checkout` on some
    //     other provider's load is none of this pass's business.
    (let src = """
import zarr as z

let store = z.load("s")
let ck = store.checkout("main")
"""
     let raw = parse src
     match ProviderDesugar.expand raw with
     | Ok p ->
         check "no icechunk import: fast path returns the SAME program"
             (LanguagePrimitives.PhysicalEquality p raw) "a new Program was built"
         check "no icechunk import: checkout node untouched"
             (isCheckout (valueOf p "ck")) "rewritten anyway"
     | Error ds -> check "no icechunk import: fast path" false (msgs ds))

    // 6c. A load whose path ALREADY carries a ref is a checkout, not a repo
    //     handle -- never recorded, which is exactly what makes the pass
    //     idempotent.
    (let src = """
import icechunk as ic

let ck0 = ic.load("r@branch:main")
let ck1 = ck0.checkout("other")
"""
     let raw = parse src
     match ProviderDesugar.expand raw with
     | Ok p ->
         check "'@' in the load path: not a repo handle, nothing recorded"
             (LanguagePrimitives.PhysicalEquality p raw) "a new Program was built"
         let l = loadOf p "ck0"
         check "'@' in the load path: the load itself is untouched"
             (l = Some ("ic", "r@branch:main")) (shown l)
     | Error ds -> check "'@' in the load path: left alone" false (msgs ds))

    // 6d. Rebinding the name drops the repo record.
    (let src = """
import icechunk as ic

let repo = ic.load("r")
let repo = 42
let ck = repo.checkout("main")
"""
     let raw = parse src
     match ProviderDesugar.expand raw with
     | Ok p ->
         check "a rebound name is no longer a repo handle"
             (LanguagePrimitives.PhysicalEquality p raw) "rewrote against a shadowed binding"
     | Error ds -> check "a rebound name is no longer a repo handle" false (msgs ds))

    // 6e. An '@' that is NOT a refspec separator: Windows and object-store
    //     paths carry '@' for unrelated reasons (a user directory `o@corp`,
    //     an `s3://key@host` form), so a bare `path.Contains "@"` test would
    //     misread them as already-checkout paths. A refspec suffix is
    //     `@<kind>:<name>`, and only the last '@' can begin one.
    printfn "\n--- '@' in a repo path is not a refspec ---"
    (let src = """
import icechunk as ic

let repo = ic.load("C:/Users/o@corp/data/w.icechunk")
let ck = repo.checkout("main")
"""
     let raw = parse src
     match ProviderDesugar.expand raw with
     | Ok p ->
         check "'@' in a directory name: the repo IS recorded (program rewritten)"
             (not (LanguagePrimitives.PhysicalEquality p raw))
             "the program came back reference-equal, so nothing was recorded"
         let l = loadOf p "ck"
         check "'@' in a directory name: the checkout rewrites, path kept verbatim"
             (l = Some ("ic", "C:/Users/o@corp/data/w.icechunk@?:main")) (shown l)
         check "'@' in a directory name: no `checkout` node survives"
             (not (isCheckout (valueOf p "ck"))) ""
     | Error ds -> check "'@' in a directory name: desugars without diagnostics" false (msgs ds))

    // The marker form over the same path, so the rewrite is not accidentally
    // specific to the bare kind.
    (let src = """
import icechunk as ic

let repo = ic.load("s3://bucket/user@host/w.icechunk")
let ck = repo.checkout("v1.0", ic.tag)
"""
     match ProviderDesugar.expand (parse src) with
     | Ok p ->
         let l = loadOf p "ck"
         check "'@' in an object-store path: the marker form rewrites too"
             (l = Some ("ic", "s3://bucket/user@host/w.icechunk@tag:v1.0")) (shown l)
     | Error ds -> check "'@' in an object-store path: the marker form rewrites too" false (msgs ds))

    // IDEMPOTENCE over the widened rule: the key the pass mints for an
    // '@'-carrying path must still read as a checkout on the next funnel, or
    // a second pass records it as a fresh repo handle with a doubled suffix.
    (let src = """
import icechunk as ic

let repo = ic.load("C:/Users/o@corp/data/w.icechunk")
let ck1 = repo.checkout("main")
let ck2 = repo.checkout("v1.0", ic.tag)
"""
     match ProviderDesugar.expand (parse src) with
     | Ok p1 ->
         (match ProviderDesugar.expand p1 with
          | Ok p2 ->
              check "'@' path: a second pass over the rewritten keys is a no-op"
                  (LanguagePrimitives.PhysicalEquality p2 p1)
                  "the rewritten key was read as a repo handle and rewritten again"
              check "'@' path: the keys survive the second pass unchanged"
                  (loadOf p2 "ck1" = Some ("ic", "C:/Users/o@corp/data/w.icechunk@?:main")
                   && loadOf p2 "ck2" = Some ("ic", "C:/Users/o@corp/data/w.icechunk@tag:v1.0"))
                  (shown (loadOf p2 "ck1") + " / " + shown (loadOf p2 "ck2"))
          | Error ds -> check "'@' path: a second pass over the rewritten keys is a no-op" false (msgs ds))
     | Error ds -> check "'@' path: the first pass succeeds" false (msgs ds))

    // The original decline still holds for every kind: a path whose '@'
    // really does introduce `<kind>:<name>` is a checkout, not a repo handle
    // (6c above pins the bare form; widening the rule must not reopen it).
    for kind in [ "?"; "branch"; "tag"; "snapshot" ] do
        let src =
            sprintf """
import icechunk as ic

let ck0 = ic.load("r@%s:main")
let ck1 = ck0.checkout("other")
"""
                    kind
        let raw = parse src
        match ProviderDesugar.expand raw with
        | Ok p ->
            check $"a '@{kind}:' suffix still reads as a checkout, not a repo handle"
                (LanguagePrimitives.PhysicalEquality p raw) "a new Program was built"
        | Error ds -> check $"a '@{kind}:' suffix still reads as a checkout" false (msgs ds)

    // ---------------------------------------------------------------
    // 7. Idempotence -- the pass runs at more than one funnel
    // ---------------------------------------------------------------
    printfn "\n--- idempotence ---"
    (match ProviderDesugar.expand (parse repoSrc) with
     | Ok p1 ->
         (match ProviderDesugar.expand p1 with
          | Ok p2 ->
              check "a second pass is a no-op (reference-equal)"
                  (LanguagePrimitives.PhysicalEquality p2 p1)
                  "rewrote an already-desugared program"
          | Error ds -> check "a second pass is a no-op" false (msgs ds))
     | Error ds -> check "the first pass succeeds" false (msgs ds))

    // ---------------------------------------------------------------
    // 8. A wrong-shaped checkout on a RECOGNIZED repo: loud, coded, located
    // ---------------------------------------------------------------
    printfn "\n--- wrong-shaped checkout refuses ---"
    let badHeader = """
import icechunk as ic
import zarr as z

let repo = ic.load("r")
let name = "main"
let ck = """

    let refusal (label: string) (call: string) =
        let raw = parse (badHeader + call + "\n")
        let rawSpan = (valueOf raw "ck").Span
        match ProviderDesugar.expand raw with
        | Ok _ -> check $"{label}: refused" false "the pass accepted it"
        | Error ds ->
            check $"{label}: refused" (ds.Length = 1) $"{ds.Length} diagnostics"
            match ds with
            | [ d ] ->
                check $"{label}: coded BL3007" (d.Code = "BL3007") d.Code
                check $"{label}: located at the checkout call"
                    (d.Span = rawSpan && rawSpan.StartLine > 0)
                    $"{spanStr d.Span} vs {spanStr rawSpan}"
                check $"{label}: message steers to the accepted forms"
                    (d.Message.Contains "ic.tag" && d.Message.Contains "checkout")
                    d.Message
            | _ -> ()

    refusal "non-literal ref name" "repo.checkout(name)"
    refusal "unknown marker field" "repo.checkout(\"v1.0\", ic.bogus)"
    refusal "marker off another alias" "repo.checkout(\"v1.0\", z.tag)"
    refusal "string in the marker slot" "repo.checkout(\"v1.0\", \"tag\")"
    refusal "three arguments" "repo.checkout(\"v1.0\", ic.tag, ic.branch)"

    // ---------------------------------------------------------------
    // 9. The marker may come off ANY icechunk alias
    // ---------------------------------------------------------------
    // `import icechunk as ic` and `import icechunk as ice` name ONE module
    // twice, so `ice.tag` is the same marker constant as `ic.tag` -- the
    // marker need not come off the alias that produced the handle.
    printfn "\n--- markers off a second icechunk alias ---"
    (let src = """
import icechunk as ic
import icechunk as ice

let repo = ic.load("r")
let ck1 = repo.checkout("v1.0", ice.tag)
let ck2 = repo.checkout("main", ice.branch)
"""
     match ProviderDesugar.expand (parse src) with
     | Ok p ->
         let l1 = loadOf p "ck1"
         check "a marker off a SECOND icechunk alias rewrites"
             (l1 = Some ("ic", "r@tag:v1.0")) (shown l1)
         let l2 = loadOf p "ck2"
         check "and so does the branch marker off that alias"
             (l2 = Some ("ic", "r@branch:main")) (shown l2)
     | Error ds -> check "a marker off a SECOND icechunk alias rewrites" false (msgs ds))

    // The load may equally come off the SECOND alias and the marker off the
    // first -- the two aliases are interchangeable in BOTH directions, and the
    // rewritten node keeps the alias its own `load` used.
    (let src = """
import icechunk as ic
import icechunk as ice

let repo = ice.load("r")
let ck = repo.checkout("v1.0", ic.tag)
"""
     match ProviderDesugar.expand (parse src) with
     | Ok p ->
         let l = loadOf p "ck"
         check "the load's alias is what the rewritten node wears"
             (l = Some ("ice", "r@tag:v1.0")) (shown l)
     | Error ds -> check "the load's alias is what the rewritten node wears" false (msgs ds))

    // The widening is exactly "any ICECHUNK alias", no wider: a field off a
    // non-icechunk module is never a ref marker (section 8's `z.tag` pins the
    // same claim through the refusal helper; this states it against the
    // widened guard directly).
    (let src = """
import icechunk as ic
import zarr as z

let repo = ic.load("r")
let ck = repo.checkout("v1.0", z.tag)
"""
     match ProviderDesugar.expand (parse src) with
     | Ok _ -> check "a marker off a NON-icechunk alias still refuses" false "the pass accepted it"
     | Error ds ->
         check "a marker off a NON-icechunk alias still refuses"
             (ds |> List.forall (fun d -> d.Code = "BL3007") && ds.Length = 1)
             (msgs ds))

    // ---------------------------------------------------------------
    // 10. A checkout OUTSIDE the blessed position is named, not left to fail
    //     obliquely (detection only -- nothing is rewritten there)
    // ---------------------------------------------------------------
    printfn "\n--- checkout position refuses ---"

    /// Every diagnostic one fixture produces, as (code, message) pairs.
    let diagsOf (src: string) : (string * string) list =
        match ProviderDesugar.expand (parse src) with
        | Ok _ -> []
        | Error ds -> ds |> List.map (fun d -> d.Code, d.Message)

    /// The positional refusal's own wording -- deliberately NOT the phrase the
    /// shape refusal shares with it ("resolved at compile time"), so a test
    /// cannot pass on the wrong diagnostic.
    let isPositional (msg: string) = msg.Contains "must be a module-level binding"

    let positional (label: string) (src: string) =
        match diagsOf src with
        | [ (code, msg) ] ->
            check $"{label}: one BL3007" (code = "BL3007") code
            check $"{label}: the positional wording" (isPositional msg) msg
        | ds -> check $"{label}: exactly one positional refusal" false $"%A{ds}"

    // 10a. A function body.
    (let src = """
import icechunk as ic

let repo = ic.load("r")
function pick(x: Float) -> Float = {
    let ck = repo.checkout("main")
    x
}
"""
     positional "checkout in a function body" src)

    // 10b. A function body with no block -- the checkout IS the body.
    (let src = """
import icechunk as ic

let repo = ic.load("r")
function pick(x: Float) -> Float = repo.checkout("main")
"""
     positional "checkout as a whole function body" src)

    // 10c. Nested in a call argument at TOP level. The binding is module-level
    //      but the checkout is not its right-hand side, so no carrier sees it.
    (let src = """
import icechunk as ic

let repo = ic.load("r")
let ck = describe(repo.checkout("main"))
"""
     positional "checkout nested in a call argument" src)

    // 10d. Nested inside a lambda body.
    (let src = """
import icechunk as ic

let repo = ic.load("r")
let k = lambda(x) -> repo.checkout("main")
"""
     positional "checkout inside a lambda" src)

    // 10e. Both misplaced checkouts in one run -- the walk does not stop at
    //      the first, so one compile reports them all.
    (let src = """
import icechunk as ic

let repo = ic.load("r")
let pair = combine(repo.checkout("main"), repo.checkout("v1.0"))
"""
     let ds = diagsOf src
     check "two misplaced checkouts in one expression: both reported"
         (ds.Length = 2 && ds |> List.forall (fun (c, m) -> c = "BL3007" && isPositional m))
         $"%A{ds}")

    // 10f. CONTROL: the blessed form is untouched by the new walk.
    (let src = """
import icechunk as ic

let repo = ic.load("r")
let ck = repo.checkout("v1.0", ic.tag)
"""
     match ProviderDesugar.expand (parse src) with
     | Ok p ->
         let l = loadOf p "ck"
         check "the blessed position still rewrites, unflagged"
             (l = Some ("ic", "r@tag:v1.0")) (shown l)
     | Error ds -> check "the blessed position still rewrites, unflagged" false (msgs ds))

    // 10g. A WRONG-SHAPED checkout at the blessed position must still report
    //      exactly ONE diagnostic: the shape refusal already owns that node,
    //      and the position walk must not double-report it.
    (let src = """
import icechunk as ic

let repo = ic.load("r")
let name = "main"
let ck = repo.checkout(name)
"""
     let ds = diagsOf src
     check "a bad-shape checkout at the blessed position reports once"
         (ds.Length = 1 && ds |> List.forall (fun (c, m) -> c = "BL3007" && not (isPositional m)))
         $"%A{ds}")

    // 10h. SHADOWING: a parameter named `repo` is not the repo handle, so a
    //      `.checkout` on it is somebody else's problem (an ordinary
    //      missing-member error) and NOT this pass's refusal.
    (let src = """
import icechunk as ic

let repo = ic.load("r")
function pick(repo: Float) -> Float = {
    let ck = repo.checkout("main")
    repo
}
"""
     let ds = diagsOf src
     check "a parameter shadowing the handle is not flagged" (List.isEmpty ds) $"%A{ds}")

    // The same claim for a block-local rebinding, which travels the statement
    // walk rather than the parameter list.
    (let src = """
import icechunk as ic

let repo = ic.load("r")
function pick(x: Float) -> Float = {
    let repo = 1.0
    let ck = repo.checkout("main")
    x
}
"""
     let ds = diagsOf src
     check "a block-local rebinding of the handle is not flagged" (List.isEmpty ds) $"%A{ds}")

    // And a receiver that was never a handle stays nobody's business wherever
    // it appears -- the pass must not start refusing every `.checkout` in a
    // program that happens to import icechunk.
    (let src = """
import icechunk as ic

let repo = ic.load("r")
let other = 42
function pick(x: Float) -> Float = {
    let ck = other.checkout("main")
    x
}
"""
     let ds = diagsOf src
     check "a non-handle receiver in a function body is not flagged" (List.isEmpty ds) $"%A{ds}")

    // ---------------------------------------------------------------
    // Summary
    // ---------------------------------------------------------------
    printFooter "Provider Desugar" [$"{passed} passed"; $"{failed} failed"]
    { Block = "Provider Desugar"
      Passed = passed
      Failed = failed
      Skipped = 0
      FailedNames = List.rev failedNames }

/// The standalone verb (`blade test provider-desugar`): the same block, as an
/// exit code.
let runProviderDesugarTests () : int =
    let r = runProviderDesugarBlock ()
    if r.Failed > 0 then 1 else 0
