# EDGI_nested_iterators

## What is this?

This package provides APIs for simple and efficient parallelization for calling C/C++ functions on multidimensional data.
This code should be compiled simultaneously with whatever files you want to use these APIs in, because the templates
cannot be instantiated unless the compiler knows which instantiations you want.

## How do I compile this?

Debug and Release versions can be built by calling "make all" on the command line from their respective folders. This
package requires support for C++17, like GCC 7 or higher. A compiler bug has been found in Intel 19 which can be 
ameliorated by declaring static arrays (like symmetry vectors) in the global scope (i.e., outside of functions).

## How do I use this?

This package breaks down the aspects of parallel computing into three separate parts: 1) representations of 
multidimensional arrays, 2) the functions (closures) of interest, and 3) the iteration strategy.

First, define your multidimensional arrays. These are the explicit, C-style pointers-to-pointers style arrays, but
native C arrays can be used as well. Many multi-D variables are represented in C as 1-D arrays, and accessed with
various strides and counters. Do not do this, it will break everything.

Second, declare a nested_array_t class of the desired value type and rank, and pass the extent vector and data to
the constructor. For example, if you have declared, allocated, and initialized a 23-dimensional boolean array for input, 
and you want a 17-dimensional float array as output, create your nested arrays like this:
```cpp
    nested_array_t<bool, 23> my_input_array(input_extents, input_data);
    nested_array_t<float, 17> my_output_array(output_extents, input_data);
```
Now, declare your closure class. This is complicated, but they are mostly boilerplate aside from the code you actually
want to run. Start by inheriting from closure_base_unary_t (N-ary closures are not currently supported, but future 
support is planned) like this:
```cpp
    struct my_1337_closure : closure_base_unary_t<bool, 21, float, 15>{
        // function goes here!
    }
```
Note that this closure is two dimensions lower than the input and output arrays. This is equivalent to a two-dimensional
domain decomposition when we apply this closure to my_input_array and my_output_array. However, as long as both closure
ranks are lower than the input and output array ranks, applying the closure is still valid and will iterate correctly.
Also, the rank differences for inputs and outputs need not be the same; if they are, the operation is a "map." If the rank
difference for outputs is lower than for inputs, this is a "map reduce." Operations where the rank difference for outputs
is greater are also valid.

Next you have to override the dummy function pointer called "function" you got when you inherited from closure_base_unary_t.
This is done most easily by setting it equal to a capture-less lambda expression:
```cpp
    struct my_1337_closure : closure_base_unary_t<bool, 21, float, 15>{
    
        static constexpr const void(*function)(nested_array_t<bool, 21, ISYMMETRY>, nested_array_t<float, 15>) =
            [](nested_array_t<bool, 21, ISYMMETRY> iarray_in, nested_array_t<float, 15> oarray_in) -> const void {
                // the good stuff
            };
            
    }
```
The type of "function" MUST be void(*)(nested_array_t<hurr>, nested_array_t<durr>). Deleting the keywords "static,"
"constexpr," and/or "const" may or may not result in a nuclear war. The lambda expression MUST be capture-less, or
type conversion will fail, and the lambda's input argument types must match nested_array_t<hurr> and nested_array_t<durr>
exactly, but you may name the input and output arrays whatever you choose. Finally, the lambda MUST return a const void.

Now that the boilerplate is done, we can have our way with the input data.
```cpp
    struct my_1337_closure : closure_base_unary_t<bool, 21, float, 15>{
    
        static constexpr const void(*function)(nested_array_t<bool, 21, ISYMMETRY>, nested_array_t<float, 15>) =
            [](nested_array_t<bool, 21, ISYMMETRY> iarray_in, nested_array_t<float, 15> oarray_in) -> const void {
                
                oarray_in = do_terrible_things(iarray_in);
                
            };
            
    }
```
Note that closures may be templated if needed, but this will slightly change how the function is called later.

You are now ready to implement your iteration strategy. There are two choices here, called "object_for" and "method_for."
When called these "nested" for loops automatically index down through the input and output arrays until the ranks match 
the closure input and output ranks. The difference between object_for and method_for is that object_for loops require 
a function when declared (and can be called with input and output arrays later), and method_for loops require an input
array when declared (and can be called with a function and an output array later). This enables you to create an iteration
strategy for a particular function or input array, and then use it multiple times later. 
```cpp
    auto oloop = object_for<decltype(my_input_array),
                            decltype(my_output_array),
                            closure_base_unary_t<bool, 21, float, 15>
                            >(my_1337_closure::function);
                            
    oloop(my_input_array, my_output_array);
                            
    // or...

    auto mloop = method_for<decltype(my_input_array),
                            decltype(my_output_array),
                            closure_base_unary_t<bool, 21, float, 15>
                            >(my_input_array);
                            
    mloop(my_1337_closure::function, my_output_array);
```
## Why is it so slow?

*shrug* Try OpenMP? OpenMP is off by default, but can be turned on by adding the number of dimensions you want to 
parallelize with OpenMP to the nested iterator declaration like so...
```cpp
    auto mloop = method_for<decltype(my_input_array),
                            decltype(my_output_array),
                            closure_base_unary_t<bool, 21, float, 15>
                            2 // <-- there will be two for-loops that will be parallelized with OpenMP
                            >(my_input_array);
```
## I can't even...

*le sigh* Examples and speed tests are provided in main.cpp.
