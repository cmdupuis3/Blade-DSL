// Unified diagnostics: the one record every compiler phase's errors converge
// to, the BLxxxx code registry, and the renderers (rustc-style snippet form
// and the legacy one-line form). Sits immediately after Ast.fs in compile
// order so every later phase can construct Diagnostics directly.
//
// Code bands (band = phase; registry below is the source of truth):
//   BL0xxx lexer          BL1xxx parser         BL2xxx name resolution
//   BL3xxx types          BL4xxx constraints/static
//   BL5xxx elaborators (50 ml, 51 ppl, 52 math, 53 rand, 54 spectra, 55 grad)
//   BL6xxx IR validation  BL7xxx backend limits
//   BL8xxx runtime (stamped into generated C++)
//   BL9xxx internal compiler errors
module Blade.Diagnostics

open System
open Blade.Ast

// Core types.

type Severity =
    | SevError
    | SevWarning
    | SevNote

type Phase =
    | PhLex
    | PhParse
    | PhResolve
    | PhTypes
    | PhConstraints
    | PhElaborate of string   // "ml" | "ppl" | "math" | "rand" | "spectra" | "grad"
    | PhIRValidate
    | PhBackend
    | PhRuntime
    | PhInternal

type Diagnostic = {
    Code: string                          // "BL3001"
    Severity: Severity
    Phase: Phase
    Span: Span                            // noSpan allowed; renderers degrade gracefully
    Message: string                       // one-line primary message
    Notes: (Span option * string) list    // secondary labels / "help:" lines
    Context: string list                  // innermost-first (CompileError.Context convention)
}

/// Carrier for phases that signal failure by exception rather than Result
/// (codegen feature limits, internal invariants). Caught at the CLI boundary.
exception BladeDiagnosticException of Diagnostic

// SourceMap: original source retained to error-report time for snippets.

type SourceMap = { Files: Map<string, string[]> }

module SourceMap =
    let empty : SourceMap = { Files = Map.empty }

    let private splitLines (source: string) : string[] =
        source.Replace("\r\n", "\n").Split('\n')

    let ofSources (sources: (string * string) list) : SourceMap =
        { Files = sources |> List.map (fun (f, s) -> f, splitLines s) |> Map.ofList }

    let addFile (file: string) (source: string) (sm: SourceMap) : SourceMap =
        { Files = Map.add file (splitLines source) sm.Files }

    let tryLines (sm: SourceMap) (file: string) : string[] option =
        Map.tryFind file sm.Files

    /// Lines for a span's file. A span with File = None (the common case)
    /// resolves against a single-file map -- the usual CLI situation.
    let tryLinesFor (sm: SourceMap) (file: string option) : string[] option =
        match file with
        | Some f -> tryLines sm f
        | None ->
            match Map.toList sm.Files with
            | [ (_, lines) ] -> Some lines
            | _ -> None

// Constructors.

let mkDiagnostic code severity phase span message : Diagnostic =
    { Code = code; Severity = severity; Phase = phase; Span = span
      Message = message; Notes = []; Context = [] }

let mkError code phase span message : Diagnostic =
    mkDiagnostic code SevError phase span message

/// Warning mirror of `mkError`. Non-fatal, but coded and spanned like every
/// other diagnostic, so `Render.render` treats it identically -- the checker's
/// `emitWarning` channel builds these.
let mkWarning code phase span message : Diagnostic =
    mkDiagnostic code SevWarning phase span message

let withNote (note: string) (d: Diagnostic) : Diagnostic =
    { d with Notes = d.Notes @ [ (None, note) ] }

let withNoteAt (span: Span) (note: string) (d: Diagnostic) : Diagnostic =
    { d with Notes = d.Notes @ [ (Some span, note) ] }

let withContext (context: string list) (d: Diagnostic) : Diagnostic =
    { d with Context = context }

// Build-output provenance.

/// Does this path name a file inside the stdlib copy deployed BESIDE the
/// running binary (bin/<config>/<tfm>/stdlib/...), rather than a stdlib the
/// user can edit?
///
/// Deliberately narrower than "anywhere under AppContext.BaseDirectory": the
/// corpus is deployed there too, and a suite run from the output directory
/// would otherwise decorate every golden diagnostic with the note below.
let private isDeployedStdlibPath (file: string) : bool =
    try
        let root =
            System.IO.Path.GetFullPath (System.IO.Path.Combine(AppContext.BaseDirectory, "stdlib"))
            + string System.IO.Path.DirectorySeparatorChar
        (System.IO.Path.GetFullPath file).StartsWith(root, StringComparison.OrdinalIgnoreCase)
    with _ -> false

/// The note a diagnostic earns when its span lands in that copy. Derived from
/// the span at render time rather than attached at construction, because it is
/// a fact about WHERE the file came from, not about what the phase found.
///
/// Worth the special case because the raw message is actively misleading: the
/// location is a path the user did not write and cannot fix by editing, and the
/// message is usually a symptom of version skew rather than a real defect --
/// either the copy is stale (the build did not refresh it) or the binary is
/// (the stdlib now uses something this compiler does not have). The note names
/// both possibilities, since the text of the error does not distinguish them.
let buildOutputNote (span: Span) : string option =
    match span.File with
    | Some f when isDeployedStdlibPath f ->
        Some ("this location is the stdlib copy deployed beside Blade.exe, not a file you wrote -- \
               the compiler and the stdlib are probably out of step. Rebuild (`dotnet build Blade.fsproj -c Release`) \
               and re-run; if it survives that, the binary predates the stdlib it is reading. \
               `blade doctor` reports which stdlib root answered")
    | _ -> None

// Code registry: every code the compiler can emit, with a short title.
// Test_Diagnostics asserts shape and uniqueness; emitting an unregistered
// code is a bug the corpus tests catch.

module Codes =
    /// Source of truth, as a LIST so duplicate codes stay visible -- building
    /// the Map directly would silently drop all but the last entry for a
    /// repeated code. Test_Diagnostics asserts this list has no duplicate codes.
    let registryEntries : (string * string) list =
        [
            // BL0xxx: lexer
            "BL0001", "unknown character"
            "BL0002", "unterminated string"
            "BL0003", "invalid numeric literal"
            "BL0999", "lexical error"
            // BL1xxx: parser
            "BL1001", "expected token"
            "BL1002", "unexpected end of file"
            "BL1999", "parse error"
            // BL2xxx: name resolution
            "BL2001", "unbound variable"
            "BL2002", "unknown qualified name"
            "BL2003", "invalid import"
            // BL2004..BL2006: FILE-based module resolution (ModuleResolve.fs),
            // the layer that turns `import units.SI` into units/SI.blade before
            // the checker ever sees the program. Separate from BL2003, which is
            // the checker's verdict on an import it CAN see.
            "BL2004", "module not found"
            "BL2005", "import cycle"
            "BL2006", "duplicate module"
            // BL2007: a provider's NATIVE library (libnetcdf) failed to load
            // while reading store metadata at `alias.load(path)`. Not BL2004
            // (the store file is not the problem) and deliberately not left
            // to the checker's opaque fallback: without metadata the store's
            // element types default and die later as a baffling BL3001 far
            // from the cause. Missing/unreadable STORES keep their lowering
            // diagnostics; this fires only for the library itself.
            "BL2007", "provider native library unavailable"
            // BL2008: the STORE named at a provider load/checkout site cannot be
            // resolved -- missing repo, bad/ambiguous ref, deleted-tag tombstone,
            // unsupported spec byte, Offline status, or a snapshot the reader
            // refuses by name. Sibling of BL2007 (there the LIBRARY is unusable,
            // here the STORE) -- both are name resolution failing, not a type
            // judgment.
            //
            // Providers opt in by raising Types.ProviderResolutionError; one
            // that doesn't keeps the historical silent fallback at check and
            // its own diagnostics at lowering.
            "BL2008", "provider cannot resolve the store"
            // BL2009: a second top-level `function` declaration reusing a name
            // the same module scope already declared. Previously the later
            // declaration silently shadowed the earlier one and calls matching
            // the first signature died blaming the caller. Same-scope only --
            // nested `function`s desugar to block lets and may still shadow an
            // outer name. Refused until same-name clause dispatch exists
            // (plan-match-statements.md §5 R1).
            "BL2009", "duplicate function declaration"
            // BL3xxx: types
            "BL3001", "type mismatch"
            "BL3002", "arity mismatch"
            "BL3003", "invalid application"
            "BL3004", "pattern type mismatch"
            "BL3005", "invalid array capture"
            "BL3006", "unit mismatch"
            "BL3007", "invalid builtin argument"
            "BL3008", "struct construction error"
            "BL3009", "rank deduction violation"
            // BL3010: strict quantity slots. A parameter whose declared type
            // carries a QUANTITY (nominal unit, `Unit speed: mps`) rejects
            // any argument not ascribed with that quantity — bare values and
            // structurally-dimensioned values alike. Split from BL3006: the
            // dims may agree perfectly; what is missing is the caller's
            // ASSERTION, and the fix is an ascription, not a conversion.
            "BL3010", "quantity argument needs ascription"
            // BL3011: quantity names are TERMINAL in unit algebra — the
            // nominal layer is exactly one level deep, so `Unit x = speed*m`
            // and `Unit q: speed` are declaration-site errors.
            "BL3011", "quantity name is terminal"
            // BL3012: parameter-default declaration rules. Defaults are
            // TRAILING (a required param may not follow a defaulted one) and
            // may reference the REQUIRED params only — they evaluate at call
            // entry, left-to-right, with just the required arguments bound.
            "BL3012", "invalid parameter default"
            // BL3013: factory declarations. Within one function's defaulted
            // trailing group, quantity-typed slots must carry DISTINCT
            // quantities — by-nominal routing needs each nominal to name
            // exactly one slot. Declaration-site, fires even if never called.
            "BL3013", "factory quantity slots must be distinct"
            // BL3014: by-nominal argument routing at a call site — a slot
            // filled twice, a tag matching no slot, or an untagged positional
            // argument after a tagged one (ambiguous mix).
            "BL3014", "invalid quantity-tagged argument routing"
            // BL3015: a name on the RHS of a `Unit` declaration that is
            // neither a declared unit nor a built-in scale constant. A unit
            // RHS composes what is already in scope; the alternative (mint
            // the declared name as a fresh base unit) types a misspelling
            // into a silently wrong dimension.
            "BL3015", "unknown unit name"
            // BL3016: a parameter and its argument BOTH carry a literal extent
            // on the same index slot, and they differ. Not a naming quibble:
            // codegen treats a literal parameter extent as ground truth and
            // bakes it into loop bounds and result allocations, so the emitted
            // C++ indexes past the argument's allocation. A symbolic extent
            // (`Idx<n>`) reads `.extents[d]` at runtime and stays permissive.
            "BL3016", "argument extent mismatch"
            // BL3017: a `group_keys` result used anywhere but its own `let`
            // RHS or a `group_by` grouping slot. The result is NAME-KEYED:
            // codegen stores the CSR structure in locals suffixed off the
            // binding name and gives the binding itself an opaque sentinel,
            // so every indirection (alias, tuple, parameter, return) used to
            // emit C++ naming symbols that were never declared. Refuse it
            // where the source name is still in hand.
            "BL3017", "group_keys result escapes its binding"
            // BL3018: a field ACCESS naming a field the struct does not
            // declare. Separate from BL3008 ("struct construction error"),
            // which is the constructor-side twin: an access had no
            // declaration check at all and degraded to a fresh type
            // variable, so `sample.vars.xdim` on a NetCDF coordinate
            // variable (filed under `.dims`) typechecked and produced an
            // array of unknown extent that only failed much later, in the
            // provider emitter, with a message about runtime extents.
            "BL3018", "unknown struct field"
            // BL3019: an explicit numeric cast (`Float32(x)`, `Int64(...)`)
            // the type system refuses: a complex source into a real/int
            // target (project with real/imag/abs first), a float->int cast
            // whose rounding is not visible at the cast site (only
            // `Int64(floor(x))` / `Int64(ceil(x))` truncate on license), a
            // non-numeric operand, an operand whose type is not yet known,
            // or a wrong arity.
            "BL3019", "invalid numeric cast"
            // BL3020: WARNING. Mixed-elem-type arithmetic silently converts
            // one operand (Int64 beside Float64 goes to float; Float32
            // beside Float64 drags the op to Float64; narrow complex widens).
            // Literals adapt silently by design -- this fires only when a
            // NON-literal operand is converted, and names the explicit cast
            // that says it out loud.
            "BL3020", "implicit numeric conversion"
            // BL3021: a `match` on a specialization index (`arity(p)` over a
            // pack param, `rank(p)` over an abstract/caret param) carries an
            // arm the specializer cannot decide statically -- a guarded arm,
            // a non-integer pattern, or a second catch-all. Arm selection
            // happens when the specialization is cloned; a guard reads
            // runtime values, so a guarded matching arm would bail the
            // constant-match fold and (for a recursive pack) let
            // specialization shrink past the base arm. The condition belongs
            // INSIDE the chosen arm's body.
            "BL3021", "undecidable specialization-index match arm"
            "BL3999", "type error"
            // BL4xxx: constraints / static
            "BL4001", "constraint violation"
            "BL4002", "static evaluation failure"
            "BL4003", "index type violation"
            "BL4004", "symmetry violation"
            "BL4005", "immutable assignment"
            "BL4006", "mutual group violation"
            "BL4007", "no equivariant map exists"
            "BL4008", "equivariance discipline violation"
            "BL4009", "galilean discipline violation"
            "BL4010", "confirm-and-pin storage suggestion"
            // BL4011: certificate-SUGGESTION channel for deduced equivariance
            // ("propose, don't export"). MLEquiv runs the shipped checking
            // judgment speculatively on uncertified functions and proposes
            // the pin. Always a warning -- an uncertified function is
            // correct, just not proved equivariant, so nothing here can fail
            // a build (and `--strict-pins`, which owns BL4010's storage
            // decision, deliberately grows no BL4011 arm).
            "BL4011", "equivariance certificate suggestion"
            "BL4012", "permutation-equivariance discipline violation"
            // BL4013: the CONTRADICTION errors, split out of BL3007's
            // ~24-way "invalid builtin argument" bucket (TypeEnv.fs). A declared
            // `comm` over a provably antisymmetric pair (or `antisymm` over a
            // provably invariant one) is not a bad builtin argument: it is an
            // annotation the deduction can prove wrong, and the one error whose
            // fix is "remove the clause / wrap in reynolds".
            "BL4013", "symmetry annotation contradicts body"
            // BL4014: BL4011's galilean twin. The inference pass runs the
            // shipped galilean judgment speculatively (try-each-velocity-
            // parameter) and proposes `where ml.galilean(u)`. Always a
            // warning, for BL4011's reason, and like BL4011 grows no
            // `--strict-pins` arm: certificates own no storage decision.
            "BL4014", "galilean certificate suggestion"
            // BL4015: the compact-class INHERITANCE gate (AntisymMapNotOdd /
            // HermitianMapNotReal / WreathTieKernelNotOdd).
            "BL4015", "compact-class inheritance not certified"
            // BL4016: `where ... omp` on a fold kernel with no reorder licence.
            // Distinct from BL4013 (annotation contradicts body): nothing is
            // disproved, there is simply no commutativity/associativity claim
            // to stand on. See docs/plan-cpp-perf-exploitation.md.
            "BL4016", "parallel fold needs a reorder licence"
            // BL4017: a declared `comm`/`anticomm` group that is provably INERT
            // at its apply -- the commuting slots are filled by virtual
            // `range<...>` operands, which never materialize and so can never
            // form the identity group compaction keys on. Neither BL4013
            // (nothing is disproved) nor BL4016 (nothing unsound is licensed):
            // the clause is simply dropped, and the emitted C++ is identical to
            // one that never carried it. Warning, on BL4001's dropped-`omp`
            // precedent -- the program is correct, just not the one asked for.
            "BL4017", "symmetry clause licenses nothing here"
            // BL4018: a ragged array LITERAL whose row lengths disagree with
            // the lens its closed `RaggedIdx<lens>` annotation names -- or
            // whose lens is not a compile-time value at all. Ragged
            // construction bakes its lens/offsets from the literal's own
            // nesting and reads the annotation's lens nowhere, so both cases
            // used to be accepted and then ignored.
            "BL4018", "ragged lens contradicts the literal"
            // BL4019: a window read `A(w(o))` whose literal offset lies
            // outside the halo's declared offset set. The interior shrink
            // covers the declared reach only, so the read lands past the
            // pool at the boundary -- silently compiled, a panic
            // interpreted. Judged for literal offsets at the apply seam
            // (haloExtentClash's site walk); computed offsets stay fail-open.
            "BL4019", "halo offset outside the declared set"
            // BL4020: `range<R>` over a `static struct` whose constraints
            // are not linear inequalities on the fields AND whose box is over
            // the cap for enumerating the solutions by table
            // (docs/plans/structural/06). A linear domain enumerates in
            // closed form and is not capped; a small non-linear one is
            // tabled with a BL4010 advisory.
            "BL4020", "constrained domain not enumerable"
            // BL4021: a `TreeIdx<shape>` whose shape is not a well-formed
            // preorder degree sequence. TWO mistakes under one code (BL4018's
            // shape one entry up): the shape does not statically evaluate at all
            // -- a tree shape is a `let static` value and from nowhere else, so a
            // run-time shape can be neither honoured nor compared, and that lane
            // is the future DynTreeIdx -- or it evaluates and is not a tree: a
            // negative or non-integer entry, an empty sequence, a walk that
            // closes before the sequence ends (a tail belonging to no tree), or
            // a sequence that ends before the walk closes (child slots nothing
            // fills). The message names which.
            "BL4021", "invalid tree shape"
            // BL5xxx: elaborators
            "BL5000", "ml elaboration error"
            "BL5100", "ppl elaboration error"
            "BL5200", "math elaboration error"
            "BL5300", "rand elaboration error"
            "BL5400", "spectra elaboration error"
            "BL5500", "grad elaboration error"
            // BL5501: the forward-mode sibling. A distinct code (not BL5500)
            // so a reject test can assert WHICH transform refused -- the two
            // subsets deliberately diverge (overwrites, if/match) past v1.
            "BL5501", "jvp elaboration error"
            // BL5502: an arity-polymorphic (`Poly<...>`) kernel that cannot be
            // UNROLLED at the arity its apply site asks for -- the recursion
            // reached an arity with no guard-free `match arity(...)` arm, it
            // destructured a pack that was already empty, or it ran past the
            // unroll budget without shrinking. Distinct from BL5500/BL5501
            // because it is a property of the KERNEL at that arity, identical
            // in forward and reverse mode, and fixed in the kernel (add a base
            // arm) rather than in the differentiated function.
            "BL5502", "arity-polymorphic kernel cannot be unrolled at this arity"
            "BL5600", "sgs elaboration error"
            "BL5700", "display elaboration error"
            // BL6xxx: IR validation
            "BL6001", "IR validation error"
            // BL7xxx: backend limits
            "BL7001", "feature not yet supported by this backend"
            "BL7002", "CUDA backend limit"
            "BL7003", "MPI backend limit"
            // A DELIBERATE codegen refusal (codegen understood the construct
            // and declined, with guidance in the message) surfaced through the
            // compile driver -- the same messages the generated `#error`
            // directives carry, delivered as a coded, spanned diagnostic
            // instead of a g++ preprocessor error. BL7001 stays the
            // unhandled-node (back-end gap) code.
            "BL7004", "construct not supported by the C++ backend"
            // BL8xxx: runtime (generated C++)
            "BL8001", "constraint violation"
            "BL8002", "non-exhaustive match"
            "BL8003", "empty reduction"
            "BL8004", "MPI runtime error"
            "BL8005", "unhandled runtime exception"
            "BL8006", "index out of bounds"
            // solve(A, b) hit an exactly-zero pivot. Raised identically by the
            // emitted LU loop nest, by the LAPACK ?gesv arm's non-zero `info`,
            // and by the interpreter's twin -- see CodeGen.solveSingularMessage.
            "BL8007", "singular matrix"
            // A scalar math intrinsic was called outside its domain: lgamma or
            // digamma at x <= 0, which refuse rather than returning a silent
            // NaN; raised identically by blade_rt::lgamma / blade_rt::digamma
            // (blade_runtime.hpp) and by the interpreter's twins
            // (Interp/Numerics.lgammaLanczos / digammaSeries).
            "BL8008", "math intrinsic domain error"
            // A halo window's declared inner extent disagrees with the RUNTIME
            // extent of an array read through the window (a group_by count,
            // typically). The compile-time twin is BL3016's HaloExtentMismatch;
            // this guard covers the extents typecheck cannot see. Emitted once
            // before the nest by genApplyCombinator's haloExtentGuards, and
            // mirrored by the interpreter's halo loop.
            "BL8009", "halo extent mismatch"
            // A recursive array's `while` guard was still true at the end of
            // the recursion budget (the declared leading extent): the
            // recurrence did not converge, and freezing the last slice would
            // silently pretend it did. Emitted by inferRecArray's budget
            // check (an IRConstraintCheck after the recursion loop) and
            // mirrored by the interpreter through the same node.
            "BL8010", "recursion budget exhausted"
            // Two operands co-iterated in one loop nest (`a + b`, `zip(a, b)`,
            // gram's contracted axis) disagree on the RUNTIME extent of a
            // shared axis. The compile-time twin is BL3016 / coIterClash,
            // which sees literal extents only; inside a function over `T^k`
            // parameters the extents are the caller's, and a curried or
            // let-bound partial application never reaches the call-site
            // check at all. Emitted once before the nest by
            // genApplyCombinator (and by materializeGramForm for the
            // contracted axis), mirrored by the interpreter's materializeApply
            // and gramArray.
            "BL8011", "co-iteration extent mismatch"
            // A provider variable's RUNTIME shape disagrees with the extents
            // lowering baked from the file present at compile time: a
            // different rank, or a dimension of a different length. The dense
            // readers hand libnetcdf a buffer sized from the baked extents and
            // `nc_get_var_*` writes the variable's whole current extent into
            // it, so a grown file overran the buffer and a shrunk or reordered
            // one read garbage into indexed cells, both silently. Emitted by
            // CppNetcdf.ncShapeGuard between `nc_inq_varid` and the read
            // (dense, compound + mask, stream open), and mirrored by the
            // interpreter's materializeProviderRead.
            "BL8012", "provider shape mismatch"
            // BL9xxx: internal compiler errors
            "BL9001", "internal compiler error"
            "BL9002", "internal codegen invariant violated"
            "BL9003", "internal lowering invariant violated"
            // Two independent judgments of the same equivariance theorem, the
            // elaboration-seam checker and the typecheck-resident walker,
            // reached contradictory verdicts. The seam has already accepted
            // the program, so this can never be the user's fault.
            "BL9004", "internal deduction disagreement"
        ]

    /// Lookup view of registryEntries. Keep entries as the thing you edit.
    let registry : Map<string, string> = Map.ofList registryEntries

    let isRegistered (code: string) = Map.containsKey code registry

    /// Phase implied by a code's band (BL0xxx lex ... BL9xxx internal).
    let phaseOfCode (code: string) : Phase =
        if code.Length < 3 then PhInternal
        else
            match code.[2] with
            | '0' -> PhLex
            | '1' -> PhParse
            | '2' -> PhResolve
            | '3' -> PhTypes
            | '4' -> PhConstraints
            | '5' ->
                match code with
                | "BL5000" -> PhElaborate "ml"
                | "BL5100" -> PhElaborate "ppl"
                | "BL5200" -> PhElaborate "math"
                | "BL5300" -> PhElaborate "rand"
                | "BL5400" -> PhElaborate "spectra"
                | "BL5500" -> PhElaborate "grad"
                | "BL5501" -> PhElaborate "grad"   // jvp runs inside the grad pass
                | "BL5502" -> PhElaborate "grad"   // the pack unroller runs there too
                | "BL5600" -> PhElaborate "sgs"
                | "BL5700" -> PhElaborate "display"
                | _ -> PhElaborate "ml"
            | '6' -> PhIRValidate
            | '7' -> PhBackend
            | '8' -> PhRuntime
            | _ -> PhInternal

    /// Elaborator stage name -> its band's generic code.
    let elaboratorCode (stage: string) =
        match stage with
        | "ml" -> "BL5000"
        | "ppl" -> "BL5100"
        | "math" -> "BL5200"
        | "rand" -> "BL5300"
        | "spectra" -> "BL5400"
        | "grad" -> "BL5500"
        | "sgs" -> "BL5600"
        | "display" -> "BL5700"
        | _ -> "BL5000"

    let ice (message: string) : Diagnostic =
        mkError "BL9001" PhInternal noSpan
            $"internal compiler error: {message}"
        |> withNote "this is a bug in the Blade compiler, not in your program -- please report it"

    let iceCodegen (message: string) : Diagnostic =
        { ice message with Code = "BL9002" }

    let backendLimit (span: Span) (message: string) : Diagnostic =
        mkError "BL7001" PhBackend span message

    /// A deliberate codegen refusal (see BL7004's registry note). The message
    /// carries its own what-to-write-instead guidance, so no generic note is
    /// attached -- unlike BL7001, this is a statement about the program's
    /// shape, not a request to report a compiler gap.
    let backendRefusal (span: Span) (message: string) : Diagnostic =
        mkError "BL7004" PhBackend span message

// Rendering.

module Render =

    let private sevLabel = function
        | SevError -> "error"
        | SevWarning -> "warning"
        | SevNote -> "note"

    // ANSI styling (used only when the caller says the sink is a TTY).
    let private styled useColor (code: string) (s: string) =
        if useColor then sprintf "[%sm%s[0m" code s else s
    let private bold useColor s = styled useColor "1" s
    let private sevColor useColor sev s =
        match sev with
        | SevError -> styled useColor "1;31" s     // bold red
        | SevWarning -> styled useColor "1;33" s   // bold yellow
        | SevNote -> styled useColor "1;36" s      // bold cyan
    let private gutterColor useColor s = styled useColor "1;34" s   // bold blue

    let private hasLocation (span: Span) = span.StartLine > 0

    let private location (span: Span) =
        let line = span.StartLine
        let col = max 1 span.StartCol
        match span.File with
        | Some f -> $"{f}:{line}:{col}"
        | None -> $"{line}:{col}"

    /// Legacy one-line form, mirroring TypeEnv.formatCompileError's shape:
    ///   "file:line:col: message" + indented context lines (outermost first).
    let renderShort (d: Diagnostic) : string =
        let loc = if hasLocation d.Span then location d.Span else ""
        let context =
            d.Context
            |> List.rev
            |> List.map (sprintf "  %s")
            |> String.concat "\n"
        if loc = "" && context = "" then d.Message
        elif context = "" then $"{loc}: {d.Message}"
        elif loc = "" then $"{d.Message}\n{context}"
        else $"{loc}: {d.Message}\n{context}"

    /// Snippet block for one located span: gutter, source line, underline.
    /// Renders the span's first line only; a multi-line span underlines to
    /// the end of that line. Returns [] when no source is available. `sev`
    /// is threaded in so the carets match the header's severity color --
    /// a mismatch would paint a warning's underline bold-red under a
    /// bold-yellow `warning[...]` label, invisible to the renderer goldens
    /// (all useColor = false) but visible to every human.
    let private snippet useColor (sev: Severity) (sm: SourceMap option) (span: Span) : string list =
        match sm |> Option.bind (fun m -> SourceMap.tryLinesFor m span.File) with
        | None -> []
        | Some lines ->
            let lineNo = span.StartLine
            if lineNo < 1 || lineNo > lines.Length then []
            else
                let text = lines.[lineNo - 1]
                let width = (string lineNo).Length
                let pad = String.replicate width " "
                let startCol = max 1 span.StartCol
                let underlineLen =
                    if span.EndLine = span.StartLine && span.EndCol > startCol
                    then span.EndCol - startCol
                    elif span.EndLine > span.StartLine
                    then max 1 (text.Length - startCol + 1)
                    else 1
                // Clamp to the visible line so a stale span cannot overflow.
                let startCol = min startCol (text.Length + 1)
                let underlineLen = max 1 (min underlineLen (text.Length - startCol + 2))
                let gut = gutterColor useColor
                [ $"""{(gut (pad + " |"))} {""}"""
                  $"""{(gut $"{lineNo} |")} {text}"""
                  sprintf "%s %s%s"
                      (gut (pad + " |"))
                      (String.replicate (startCol - 1) " ")
                      (sevColor useColor sev (String.replicate underlineLen "^")) ]

    /// Full rustc-style rendering:
    ///   error[BL3001]: message
    ///     --> file:line:col
    ///      |
    ///    3 |     offending line
    ///      |     ^^^^^^^
    ///      = note: ...
    ///   (context lines, outermost first, as trailing notes)
    let render (useColor: bool) (sm: SourceMap option) (d: Diagnostic) : string =
        let header =
            sprintf "%s%s %s"
                (sevColor useColor d.Severity (sevLabel d.Severity))
                (sevColor useColor d.Severity $"[{d.Code}]:")
                (bold useColor d.Message)
        let locLines =
            if hasLocation d.Span then
                $"""  {(gutterColor useColor "-->")} {(location d.Span)}"""
                :: snippet useColor d.Severity sm d.Span
            else []
        let noteLines =
            d.Notes
            |> List.collect (fun (nspan, text) ->
                let noteLine = $"""  {(gutterColor useColor "=")} {$"note: {text}"}"""
                match nspan with
                | Some s when hasLocation s ->
                    noteLine :: ($"""    {(gutterColor useColor "-->")} {(location s)}""") :: []
                | _ -> [ noteLine ])
        // Synthesized last, so a phase's own notes still read first: this one
        // is about the FILE, not the finding.
        let provenanceLines =
            match buildOutputNote d.Span with
            | Some text -> [ $"""  {(gutterColor useColor "=")} {$"note: {text}"}""" ]
            | None -> []
        let contextLines =
            d.Context
            |> List.rev
            |> List.map (fun c -> $"""  {(gutterColor useColor "=")} {c}""")
        String.concat "\n" (header :: (locLines @ noteLines @ provenanceLines @ contextLines))

    let renderAll (useColor: bool) (sm: SourceMap option) (ds: Diagnostic list) : string =
        ds |> List.map (render useColor sm) |> String.concat "\n\n"
