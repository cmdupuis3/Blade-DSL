// The `check` / `checkCells` payload, transcribed field-for-field from the
// emitter at src/Ide.fs:282-483 (`renderJson`). That StringBuilder is the
// source of truth; this file is documentation of it, and drift is a bug here,
// not there.
//
// Conventions that hold everywhere in this payload:
//
//   * Positions are 1-BASED, and `endCol` is EXCLUSIVE (a one-character token
//     at column 5 spans col 5, endCol 6). `Ide.clampSpan` guarantees line and
//     col are at least 1 even for synthesized nodes.
//   * An absent value is an ABSENT FIELD, never null — the emitter simply does
//     not write it. The single exception is `references[].def`, which is
//     emitted as literal `null` when no name span survived.
//   * Field ORDER is fixed and, for the one-shot `blade ide check --json`
//     path, byte-pinned by the compiler's own tests. Never assume a client may
//     add fields to it.
//
// The same payload arrives three ways: one-shot `ide check --json` (no `id`,
// no `tier`), an `ide serve` `check` response (`id` + `tier`), and an `ide
// serve` `checkCells` response (`id` + `tier` + `windows`).

/** A 1-based span; `endCol` is exclusive. */
export interface Span {
  line: number;
  col: number;
  endLine: number;
  endCol: number;
}

/** Where one notebook cell's text landed in the assembled session source.
 *  1-based inclusive line range. `wrapLine`/`wrapCol` appear only for a cell
 *  that needed a synthetic wrapper binding (a bare expression); a cell whose
 *  definition a later cell rebound gets an EMPTY range that nothing can fall
 *  inside. src/Ide.fs:261-266, emitted at :297-312. */
export interface CellWindow {
  startLine: number;
  endLine: number;
  wrapLine?: number;
  wrapCol?: number;
}

/** One diagnostic. `code` is omitted (not empty) when the diagnostic carries
 *  no BLxxxx code. src/Ide.fs:79-88, emitted at :314-322. */
export interface Diagnostic {
  severity: "error" | "warning";
  line: number;
  col: number;
  endLine: number;
  endCol: number;
  message: string;
  /** e.g. "BL3016". Absent for diagnostics with no registered code. */
  code?: string;
}

/** One parameter of a function binding. src/Ide.fs:90-102. */
export interface Param {
  name: string;
  type: string;
  /** The parameter's line from the enclosing doc comment. Absent when empty. */
  doc?: string;
  /** Deduced minimum rank. Present only when DEDUCED — an annotated parameter
   *  shows its rank in `type` instead. */
  minRank?: number;
  /** Pretty-printed default value expression (`s: Float = 2.0` -> "2.0").
   *  Absent on required parameters. */
  default?: string;
}

/** One name the checker bound. src/Ide.fs:104-133, emitted at :324-375.
 *
 *  `line`/`col`/`endLine`/`endCol` span the DECLARATION, not the name token —
 *  go to `references[]` for name-token spans. `endLine`/`endCol` are emitted
 *  LAST so the leading field run stays byte-stable for existing clients.
 *
 *  `params`/`ret`/`where`/`deducedComm` appear together, on functions only. */
export interface Binding {
  name: string;
  /** Open string. Observed values: "function", "static function", "value",
   *  "param", "local", "type" — derived from the source-kind channel at
   *  src/Ide.fs:1449, so treat unknown values as opaque. */
  kind: string;
  line: number;
  col: number;
  /** The type as the fast tier knows it. */
  type: string;
  /** Full-tier only: the type MONOMORPHIZATION resolved, when strictly more
   *  concrete than `type`. Absent on the fast tier and where lowering changed
   *  nothing. */
  concreteType?: string;
  /** Doc comment. Absent when empty. On a function, the parameter lines are
   *  stripped out (they travel on `params[]`). */
  doc?: string;
  /** Provenance for a top-level provider READ
   *  (`let x = store.vars.v |> alias.read`; also `alias.stream`,
   *  `alias.read_window`, `alias.load_compound`): the store member this
   *  binding reads. */
  providerRead?: { store: string; member: string };
  /** Provenance for a top-level provider WRITE
   *  (`let saved = alias.write("out.csv", x)`): the store member the array it
   *  persists ORIGINALLY came from, chased through `x`, and present only when
   *  `x` itself recorded a provenance in the same module.
   *
   *  A separate field from `providerRead` because it is the opposite
   *  direction — a write binding reads nothing — and the two never appear on
   *  one binding. Clients that render `providerRead` as "reads …" must not
   *  render this one the same way. */
  providerWrite?: { store: string; member: string };
  /** Functions only. */
  params?: Param[];
  /** Functions only — the return type. Its presence is what marks this
   *  binding as a function in the payload. */
  ret?: string;
  /** Declared `where` conjuncts. Functions only, and omitted when empty. */
  where?: string[];
  /** Stage-3 DEDUCED symmetry as canonical pin-clause strings ("comm(a, b)"),
   *  declared or not. Always present on functions: `[]` means "deduction ran
   *  and proved nothing". */
  deducedComm?: string[];
  endLine: number;
  endCol: number;
}

/** A `dims` or `vars` field of a loaded provider store. */
export interface ProviderMember {
  name: string;
  type: string;
}

/** A provided named index type, with its extent when statically known. */
export interface ProviderIndexType {
  name: string;
  extent?: number;
}

/** One `let store = alias.load("path")` binding and the structure the provider
 *  derived from it. Note the array is named `providers` (not `stores`).
 *  src/Ide.fs:150-160, emitted at :376-403. */
export interface Provider {
  /** The store binding's name. */
  store: string;
  /** The provider alias the load went through. */
  alias: string;
  /** The provider's own name ("netcdf", "zarr", ...). */
  provider: string;
  /** The path literal as written. */
  path: string;
  line: number;
  col: number;
  indexTypes: ProviderIndexType[];
  dims: ProviderMember[];
  vars: ProviderMember[];
}

/** Fields every `deduced[]` entry carries. */
interface DeducedBase extends Span {
  /** Function name, or "<kernel>" for an inline kernel. */
  owner: string;
  /** Parameter index, or adjacent-pair index. */
  index: number;
}

/** A deduced minimum rank for one parameter. */
export interface DeducedRank extends DeducedBase {
  kind: "rank";
  /** The parameter name. */
  name: string;
  rank: number;
}

/** Deduced commutativity across a recursive pack. */
export interface DeducedPackComm extends DeducedBase {
  kind: "packComm";
  /** The pack name. */
  name: string;
}

/** A stage-6a ML certificate fact: `kind` is the discipline (src/Ide.fs:247-254
 *  maps `fact.Discipline` straight onto it), `name` the group, and `left` the
 *  comma-joined dependency list.
 *
 *  Each discipline gets its OWN single-literal interface rather than one
 *  interface with a `"equiv" | "galilean"` discriminant: TypeScript will not
 *  narrow a member whose discriminant is a union of literals out of the
 *  surrounding union, so the multi-literal spelling silently breaks the
 *  `else` branch of every consumer's `switch`. */
export interface DeducedEquiv extends DeducedBase {
  kind: "equiv";
  name: string;
  left: string;
}

/** Galilean-discipline twin of DeducedEquiv. */
export interface DeducedGalilean extends DeducedBase {
  kind: "galilean";
  name: string;
  left: string;
}

export type DeducedDiscipline = DeducedEquiv | DeducedGalilean;

/** A deduced commutativity between two parameters. */
export interface DeducedComm extends DeducedBase {
  kind: "comm";
  left: string;
  right: string;
}

/** A deduced anticommutativity between two parameters. */
export interface DeducedAnticomm extends DeducedBase {
  kind: "anticomm";
  left: string;
  right: string;
}

/** The pair shape. This is also the CATCH-ALL: the emitter special-cases only
 *  rank, packComm, equiv and galilean; every other `kind` — including a
 *  discipline added by a future elaborator — falls to the `else` arm at
 *  src/Ide.fs:418 and is emitted with `left`/`right`. So a `kind` this union
 *  does not name still arrives in THIS shape. */
export type DeducedPair = DeducedComm | DeducedAnticomm;

/** One fact the checker DEDUCED rather than read off an annotation. A
 *  top-level array because it can carry kernel-site facts that belong to no
 *  named binding. Only the fields meaningful for the `kind` are emitted —
 *  src/Ide.fs:404-425.
 *
 *  Discriminate on `kind`. Keep a `default` branch: `kind` is open on the wire
 *  (see DeducedPair), so a newer compiler can send one this union does not
 *  name. */
export type Deduced =
  | DeducedRank
  | DeducedPackComm
  | DeducedEquiv
  | DeducedGalilean
  | DeducedComm
  | DeducedAnticomm;

/** The defensive view, for a consumer that would rather test fields than trust
 *  `kind`: every field any arm can carry, all optional. */
export type AnyDeduced = DeducedBase & {
  kind: string;
  name?: string;
  left?: string;
  right?: string;
  rank?: number;
};

/** One builtin call site, with argument and result types pre-rendered in the
 *  compiler's concrete notation. src/Ide.fs:164-172, emitted at :426-437. */
export interface Call extends Span {
  name: string;
  args: string[];
  ret: string;
}

/** A deduced per-parameter cell rank at a kernel site. */
export interface KernelMinRank {
  param: string;
  rank: number;
}

/** One lambda-kernel site with its deduction snapshot. Span-keyed: hover and
 *  completion resolve a kernel through position, not a name.
 *  src/Ide.fs:194-203, emitted at :438-460. */
export interface Kernel extends Span {
  params: string[];
  /** Canonical pin-clause strings for what deduction proved. */
  deducedComm: string[];
  /** The `where` conjuncts actually written at the site. */
  declaredWhere: string[];
  minRanks: KernelMinRank[];
}

/** One BINDER and every use that resolves to it. Keyed internally by the
 *  binder's IRId, never by name — which is what makes two shadowing `x`s two
 *  entries with disjoint use lists instead of one merged blob. Definition,
 *  find-references and rename all read this array and nothing else.
 *  src/Ide.fs:218-227, emitted at :461-481. */
export interface Reference {
  name: string;
  kind: "function" | "value" | "param" | "local" | "type";
  /** The NAME TOKEN, not the declaration. Literally `null` when no span
   *  survived; such an entry is emitted only if it still has uses. */
  def: Span | null;
  uses: Span[];
}

/** The whole check payload. */
export interface CheckPayload {
  /** Echoed request id. Present on `ide serve` responses, absent from one-shot
   *  `ide check --json`. */
  id?: number;
  /** Echoed tier. Same presence rule as `id`. */
  tier?: "fast" | "full";
  /** One entry per input cell, in input order. `checkCells` responses only. */
  windows?: CellWindow[];
  /** Payload schema version. Always 1 today. */
  version: 1;
  diagnostics: Diagnostic[];
  bindings: Binding[];
  providers: Provider[];
  deduced: Deduced[];
  calls: Call[];
  kernels: Kernel[];
  references: Reference[];
}
