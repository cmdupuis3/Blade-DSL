// Effect summaries and the optimization decision record -- the SHARED
// legality facts of plan-fortran-killer-2.md section 3 (P0).
//
// Every rewrite in the semantic-equivalence layer (src/Optimize.fs) has to
// answer the same question before it fires: "is evaluating this callable
// again, or skipping an evaluation of it, observable?" Before this module
// each pass answered it locally -- the freeze recognizer trusted an
// invariant callee NAME (and miscompiled a guard whose callee mutated its
// own input), fusion walked IR bodies with its own module-local `pureBody`
// -- and neither could see the other's evidence. The summary below is
// computed ONCE per function declaration from the TYPED body with callees
// resolved (TypeCheckSupport.effectsOfBody), stored on the typed
// declaration and in TypeEnv.FuncEffects, grafted onto the IR callable by
// Lowering, and read by every consumer: freeze recognition at the typecheck
// seam, fusion at the IR seam, and whatever comes next (destination reuse,
// CSE). A consumer that finds `Unknown` set falls back to its own analysis
// or declines; nothing here is ever a licence to skip a check the consumer
// used to make.
//
// The lattice is deliberately small and CONSERVATIVE: each flag is a "may",
// joins are disjunctions, and an unresolvable callee sets Unknown. Two
// notions are kept apart on purpose (plan section 3 step 3): REPEATABLE
// (re-evaluating a completed evaluation changes nothing) admits MayFail,
// because a repeat of an evaluation that completed cannot newly fail;
// MOVABLE (hoist past a conditional, drop when unconsumed) does not.
//
// The decision record is the auditable half (plan section 3 step 4): each
// pass records what it decided, for which subject, why, and on what
// evidence, into an AsyncLocal collector that `blade plan` installs. With no
// collector installed (every other entry point) recording is a no-op, so the
// test harness's parallel compiles never accumulate anything.
module Blade.Effects

open Blade.Ast

/// Conservative per-callable effect summary. Every field is a "may".
type EffectSummary = {
    /// Assigns through a `mut` parameter or to a captured / module-level
    /// mutable, or performs any assignment at all inside its body.
    Mutates: bool
    /// Emits display output (print / show / the notebook frames).
    EmitsOutput: bool
    /// Reads external state at run time: a provider read (`|> read`), a
    /// streamed fiber. Deterministic for a fixed file, but not a function
    /// of the arguments alone.
    ReadsExternal: bool
    /// May abort at run time: a bounds check (BL8006), an empty reduction
    /// (BL8003), a singular solve (BL8007), a non-exhaustive match (BL8002),
    /// a value constraint (BL8001), a domain-checked intrinsic (BL8008).
    MayFail: bool
    /// Calls something whose effects are not known: a lambda-valued
    /// variable, a higher-order parameter, a callee with no summary.
    Unknown: bool
}

let noEffects : EffectSummary =
    { Mutates = false; EmitsOutput = false; ReadsExternal = false; MayFail = false; Unknown = false }
let unknown : EffectSummary = { noEffects with Unknown = true }
let mayFail : EffectSummary = { noEffects with MayFail = true }
let mutates : EffectSummary = { noEffects with Mutates = true }
let emitsOutput : EffectSummary = { noEffects with EmitsOutput = true }
let readsExternal : EffectSummary = { noEffects with ReadsExternal = true }

let join (a: EffectSummary) (b: EffectSummary) : EffectSummary =
    { Mutates = a.Mutates || b.Mutates
      EmitsOutput = a.EmitsOutput || b.EmitsOutput
      ReadsExternal = a.ReadsExternal || b.ReadsExternal
      MayFail = a.MayFail || b.MayFail
      Unknown = a.Unknown || b.Unknown }

let joinAll (xs: EffectSummary seq) : EffectSummary = Seq.fold join noEffects xs

/// REPEATABLE: evaluating the callable again on the same inputs, after an
/// evaluation that completed, yields the same value and changes nothing
/// observable. MayFail is allowed -- a repeat of a completed evaluation
/// cannot newly fail -- which is exactly what the freeze recognizer and the
/// fusion splice need.
let isRepeatable (s: EffectSummary) : bool =
    not s.Mutates && not s.EmitsOutput && not s.ReadsExternal && not s.Unknown

/// MOVABLE: repeatable AND cannot fail, so an evaluation may be hoisted past
/// a conditional or dropped when unconsumed. No pass reads this yet; it is
/// here so the two notions are never conflated by the first one that does.
let isMovable (s: EffectSummary) : bool = isRepeatable s && not s.MayFail

let describe (s: EffectSummary) : string =
    let parts =
        [ if s.Mutates then yield "mutates"
          if s.EmitsOutput then yield "emits output"
          if s.ReadsExternal then yield "reads external data"
          if s.MayFail then yield "may fail"
          if s.Unknown then yield "calls unknown code" ]
    if parts.IsEmpty then "pure" else String.concat ", " parts

// --- Decision record -------------------------------------------------------

type DecisionOutcome =
    | Applied
    | Declined of reason: string

/// One recorded optimization decision. `Rule` names the pass (stable, used
/// by tests), `Version` bumps when the rule's admission criteria change so a
/// record can be told apart from an older rule's, `Subject` is the thing
/// decided about (a recursive array's name, a kernel's name), `Evidence`
/// lists the obligations the pass discharged (Applied) or what it saw
/// (Declined).
type Decision = {
    Rule: string
    Version: int
    Span: Span
    Subject: string
    Outcome: DecisionOutcome
    Evidence: string list
}

module Decisions =
    let private cell = System.Threading.AsyncLocal<System.Collections.Generic.List<Decision>>()

    /// Install a collector for the current async flow. Only `blade plan` and
    /// tests call this; without it `record` is a no-op.
    let start () = cell.Value <- System.Collections.Generic.List<Decision>()

    let record (d: Decision) =
        match cell.Value with
        | null -> ()
        | l -> l.Add d

    /// Take every decision recorded since `start` and clear the collector.
    let drain () : Decision list =
        match cell.Value with
        | null -> []
        | l ->
            let xs = List.ofSeq l
            l.Clear()
            xs

    let render (d: Decision) : string =
        let where = if d.Span.StartLine > 0 then $" @ {d.Span.StartLine}:{d.Span.StartCol}" else ""
        let outcome =
            match d.Outcome with
            | Applied -> "applied"
            | Declined why -> $"declined -- {why}"
        let ev = if d.Evidence.IsEmpty then "" else " [" + String.concat "; " d.Evidence + "]"
        $"[{d.Rule} v{d.Version}] {d.Subject}{where}: {outcome}{ev}"

    let private jsonString (s: string) : string =
        let sb = System.Text.StringBuilder()
        sb.Append('"') |> ignore
        for c in s do
            match c with
            | '"' -> sb.Append("\\\"") |> ignore
            | '\\' -> sb.Append("\\\\") |> ignore
            | '\n' -> sb.Append("\\n") |> ignore
            | '\r' -> sb.Append("\\r") |> ignore
            | '\t' -> sb.Append("\\t") |> ignore
            | c when c < ' ' -> sb.AppendFormat("\\u{0:x4}", int c) |> ignore
            | c -> sb.Append(c) |> ignore
        sb.Append('"') |> ignore
        sb.ToString()

    /// One JSON object per plan: `{"file":..., "decisions":[{rule, version,
    /// subject, line, col, outcome, reason, evidence}]}`.
    let renderJson (file: string) (ds: Decision list) : string =
        let one (d: Decision) =
            let outcome, reason =
                match d.Outcome with
                | Applied -> "applied", "null"
                | Declined why -> "declined", jsonString why
            let ev = d.Evidence |> List.map jsonString |> String.concat ","
            $"{{\"rule\":{jsonString d.Rule},\"version\":{d.Version},\"subject\":{jsonString d.Subject},\"line\":{d.Span.StartLine},\"col\":{d.Span.StartCol},\"outcome\":\"{outcome}\",\"reason\":{reason},\"evidence\":[{ev}]}}"
        let body = ds |> List.map one |> String.concat ","
        $"{{\"file\":{jsonString file},\"decisions\":[{body}]}}"
