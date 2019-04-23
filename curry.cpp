/***********************************************************************
 *                   GNU Lesser General Public License
 *
 * This file is part of the EDGI package, developed by the 
 * GFDL Flexible Modeling System (FMS) group.
 *
 * EDGI is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at
 * your option) any later version.
 *
 * EDGI is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with EDGI.  If not, see <http://www.gnu.org/licenses/>.
 **********************************************************************/

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
