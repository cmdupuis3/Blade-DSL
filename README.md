# Blade DSL

Blade DSL is an experimental C++ extension created to support simple expression of complex array-oriented problems, while maximizing parallel performance.

Blade DSL was created for two main reasons:
  - To enable application of a higher-than-unary function to multiple sets of arrays of different ranks without worrying about rewriting the iteration patterns.
  - To enable fast calculation of full comoment tensors while preserving the original arrays' dimensions.
  
Blade DSL syntax is heavily inspired by functional languages, and applies some functional concepts to array programming to offer some unique advantages. For example, Blade DSL can deduce symmetry in output arrays based on symmetry in input arrays and commutativity in the function of interest. Due to strong dimensional typing, this symmetry deduction is automatic for all valid functions.
