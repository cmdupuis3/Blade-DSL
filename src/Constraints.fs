// Open where-clause constraint registry: the extension point that lets domain
// layers (ML, PPL) own constraint keywords the core grammar doesn't know.
// The parser records unknown `where <name>(<idents>)` conjuncts as data
// (WhereClause.Custom); the checker dispatches each conjunct through this
// registry. Sibling of StaticEval's external-builtin registry. Registration
// happens during the owning module's elaboration stage, always before
// checking, so handlers are in place by the time signatures are checked.
//
// A handler is a two-phase interface -- a function carrying a registered
// conjunct is effectively promoted to the owning module's function kind:
//   Validate            signature-site well-formedness (args vs params)
//   EnterBody/ExitBody  license scope around the declaring function's body
//                       (stack discipline: the handler owns whatever state
//                       the license affects)
//   Discharge           call-site verification -- the caller must prove the
//                       declared relation for the actual arguments, via the
//                       provenance oracle the checker supplies
module Blade.Constraints

type ConstraintHandler = {
    /// One-line description, shown in "unknown constraint" diagnostics.
    Describe: string
    /// funcName -> paramNames -> conjunct args -> well-formed?
    Validate: string -> string list -> string list -> Result<unit, string>
    /// Open a license scope for the declaring function's body.
    /// funcName -> conjunct args.
    EnterBody: string -> string list -> unit
    /// Close the license scope (always called, error paths included).
    ExitBody: string -> string list -> unit
    /// Call-site discharge. funcName -> conjunct args -> provenance oracle
    /// (callee param name |-> the caller-side provenance set of the actual
    /// argument bound to it; empty = unknown).
    Discharge: string -> string list -> (string -> Set<string>) -> Result<unit, string>
}

let private handlers =
    System.Collections.Concurrent.ConcurrentDictionary<string, ConstraintHandler>()

/// Register (or re-register -- idempotent) a constraint keyword.
let registerConstraint (name: string) (handler: ConstraintHandler) : unit =
    handlers.[name] <- handler

let lookupConstraint (name: string) : ConstraintHandler option =
    match handlers.TryGetValue name with
    | true, h -> Some h
    | _ -> None

/// Registered vocabulary, for "unknown constraint" diagnostics.
let registeredConstraintNames () : string list =
    handlers.Keys |> Seq.sort |> List.ofSeq

/// The provenance token a function parameter carries inside its declaring
/// function's body. Shared here so the checker (which seeds parameter
/// provenance) and handlers (which insert license facts over these tokens
/// in EnterBody) agree on the format without coupling to each other.
let paramProvenanceToken (funcName: string) (paramName: string) : string =
    $"{funcName}.{paramName}"

// --- `__ad_body`: the conjunct Grad stamps on every synthesized derivative ---
//
// `ad.grad` / `ad.jvp` synthesize `f__grad` / `f__jvp` as SURFACE source
// (src/Grad.fs), before typecheck, so the transform never sees units. Units
// ride the ordinary checker for free -- a tangent has the primal's type, a
// cotangent is declared as <loss>/<parameter> -- EXCEPT where the checker
// inserts a RUNTIME scale factor (convertScaleTo: `+`/`-`/comparison joins
// and annotated bindings between magnitudes of one dimension). The
// transform treats an ascription as identity and `+` as linear, so the
// derivative of a conversion by k would come out as 1, not k: a silent
// wrong answer. This conjunct marks the synthesized body; while the checker
// is inside it (`EnterBody`/`ExitBody`, per async flow) convertScaleTo
// refuses to insert a factor. Registered by the typecheck driver
// (`registerAdBody`), idempotently, before any declaration is checked.
let adBodyConjunct = "__ad_body"

let private adBodyDepth = System.Threading.AsyncLocal<int ref>()
let private adBodyCell () : int ref =
    let v = adBodyDepth.Value
    if isNull (box v) then
        let fresh = ref 0
        adBodyDepth.Value <- fresh
        fresh
    else v

/// Is the checker currently inside a synthesized derivative's body?
let inAdBody () : bool = (adBodyCell ()).Value > 0

let registerAdBody () : unit =
    registerConstraint adBodyConjunct
        { Describe = "internal: a derivative synthesized by ad.grad / ad.jvp (no implicit unit-scale conversion inside its body)"
          Validate = fun _ _ _ -> Ok ()
          EnterBody = fun _ _ -> (let c = adBodyCell () in c.Value <- c.Value + 1)
          ExitBody = fun _ _ -> (let c = adBodyCell () in c.Value <- max 0 (c.Value - 1))
          Discharge = fun _ _ _ -> Ok () }
