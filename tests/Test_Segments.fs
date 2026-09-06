// Test sources live on disk in tests/corpus; this module only names the
// category. Segmented domains (docs/plans/structural/07): `Chunked<I, K>`,
// the structural grouping `segments(A)`, `group_by` over it, `ungroup`, and
// the refusals around them.
module Blade.Tests.Segments

open Blade.Tests.Corpus

let segmentsTests = category "segments"
