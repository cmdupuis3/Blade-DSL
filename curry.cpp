
#ifndef CURRY_CPP
#define CURRY_CPP

#include <functional>
#include <vector>

#include "nested_array.cpp"

using std::vector;

/** Currying function; Use this to 'instantiate' a multifunction for a specific arity. */
/*
template<typename OTYPE, typename ITYPE>
std::function<void(int, ITYPE, OTYPE)> curry(int arity_in, void func_in(int, int, ITYPE, OTYPE)){
    return [arity_in, func_in](int num, ITYPE iarrays, OTYPE oarray){ return func_in(arity_in, num, iarrays, oarray); };
}

template<typename ITYPE, typename OTYPE, const int* ISYMMETRY, const int* OSYMMETRY>
std::function<void(nested_array_t<ITYPE, ISYMMETRY>, nested_array_t<OTYPE, OSYMMETRY>)> curry(int arity_in, void func_in(int, nested_array_t<ITYPE, ISYMMETRY>, nested_array_t<OTYPE, OSYMMETRY>)){
    return [arity_in, func_in](nested_array_t<ITYPE, ISYMMETRY> iarrays, nested_array_t<OTYPE, OSYMMETRY> oarray){ return func_in(arity_in, iarrays, oarray); };
}

template<typename ITYPE, typename OTYPE, const int* ISYMMETRY, const int* OSYMMETRY>
std::function<void(vector<nested_array_t<ITYPE, ISYMMETRY> >, nested_array_t<OTYPE, OSYMMETRY>)> curry(int arity_in, void func_in(int, vector<nested_array_t<ITYPE, ISYMMETRY> >, nested_array_t<OTYPE, OSYMMETRY>)){
    return [arity_in, func_in](vector<nested_array_t<ITYPE, ISYMMETRY> > iarrays, nested_array_t<OTYPE, OSYMMETRY> oarray){ return func_in(arity_in, iarrays, oarray); };
}
*/

/** Currying function for native arrays; Use this to 'instantiate' a multifunction for a specific arity. */
template<typename OTYPE, typename ITYPE, int arity_in, void func_in(int, int, ITYPE, OTYPE)>
constexpr std::function<void(int, ITYPE, OTYPE)> curry(){
    return [](int num, ITYPE iarrays, OTYPE oarray){ return func_in(arity_in, num, iarrays, oarray); };
}

/** Currying function for nested arrays; Use this to 'instantiate' a multifunction for a specific arity. */
template<typename ITYPE, const int irank, typename OTYPE, const int orank, const int* ISYMMETRY, const int* OSYMMETRY, int arity_in, void(func_in)(int, nested_array_t<ITYPE, irank, ISYMMETRY>, nested_array_t<OTYPE, orank, OSYMMETRY>)>
constexpr std::function<void(nested_array_t<ITYPE, irank, ISYMMETRY>, nested_array_t<OTYPE, orank, OSYMMETRY>)> curry(){
    return [](nested_array_t<ITYPE, irank, ISYMMETRY> iarrays, nested_array_t<OTYPE, orank, OSYMMETRY> oarray){ return func_in(arity_in, iarrays, oarray); };
}





#endif
