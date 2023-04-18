# Blade DSL

Blade DSL is an experimental C++ extension created to support simple expression of complex array-oriented problems, while maximizing parallel performance.

Blade DSL was created for two main reasons:
  - To enable application of a higher-than-unary function to multiple sets of arrays of different ranks without worrying about rewriting the iteration patterns.
  - To enable fast calculation of full comoment tensors while preserving the original arrays' dimensions.
  
Blade DSL syntax is heavily inspired by functional languages, and applies some functional concepts to array programming to offer some unique advantages. For example, Blade DSL can deduce symmetry in output arrays based on symmetry in input arrays and commutativity in the function of interest. Due to strong dimensional typing, this symmetry deduction is automatic for all valid functions.

## Using / "Installing"

Blade DSL requires compilers for C++ and F#. Also, the NetCDF library is currently required, but in the future it will be optional for non-NetCDF core functionalities. It is recommended to use a Linux OS with Microsoft's [Dotnet](https://dotnet.microsoft.com/en-us/download) framework installed, along with the latest versions of GCC and [NetCDF](https://downloads.unidata.ucar.edu/netcdf/). At this time, Blade DSL runs with F# 6.0. In testing, F# 7.0 was found to cause compilation errors, so if that continues to occur, F# 6.0 is recommended instead. C11 and C++17 are the standards of C and C++ used here. Note that Blade DSL *cannot* be compiled with msvc (I.e., Visual Studio) because it does not have adequate OpenMP support.

Different parts of Blade DSL are compiled in stages, which requires a somewhat unintuitive build system. An example bash script is provided as an example of how to compile a valid Blade DSL file into an executable, although we can safely say it won't be winning any beauty contests. 

This script should work out of the box as a command-line tool.

