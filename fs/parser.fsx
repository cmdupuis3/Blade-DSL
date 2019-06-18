
open System.IO

#load "parser.fs"


let code = """

#include "things.hpp"
#include "stuff.hpp"

#pragma edgi function(product) arity(any) input(iarrays) irank(0) output(oarray) orank(0)
{
    oarray = 1;
    for(int i = 0; i < arity; i++){
        oarray *= iarrays[i];
    }
}

#pragma edgi function(product3) arity(3) input(iarrays) irank(0) output(oarray) orank(0)
{
    oarray = 1;
    for(int i = 0; i < arity; i++){
        oarray *= iarrays[i];
    }
}

#pragma edgi function(sumThenMultiply) input(iarray1, iarray2, iarray3) iranks(1, 1, 0) commutativity(1, 1, 3) output(oarray) orank(0)
{
    // assume iarray1 and iarray2 last extents are same
    for(int i = 0; i < iarray1.current_extent(); i++){
        oarray += iarray1[i] + iarray2[i];
    }

    oarray *= iarray3;
    //return oarray;
}

#pragma edgi function(add10) input(iarray) iranks(0) output(oarray) orank(0)
{
    oarray = iarray + 10;
    //return oarray;
}

int main(){

    #pragma edgi array symmetry(1, 2, 2, 3)
    promote<float, 4> array1;

    #pragma edgi array
    promote<float, 3> array3;
    
    auto oloop = object_for(sumThenMultiply);
    auto mloop = method_for(array1, array1, array3);

    auto ooarray = oloop(array1, array1, array3);
    auto moarray = mloop(sumThenMultiply);

    auto noarray = nested_for(sumThenMultiply, array1, array1, array3);

    auto newfunc = pipe(sumThenMultiply, add10);


    return 0;
}

"""
