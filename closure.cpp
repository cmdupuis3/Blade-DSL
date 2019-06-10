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

#ifndef CLOSURE_CPP
#define CLOSURE_CPP

#include <functional>
#include <type_traits>
#include <vector>

using std::add_pointer;
using std::remove_pointer;
using std::vector;

#include "nested_array.cpp"
#include "nested_netcdf_array.cpp"
#include "nested_array_utilities.cpp"
#include "curry.cpp"

/** Compile-time available container for metadata about functions. */
template<const int ARITY, typename FITYPE, const int FIRANK, typename FOTYPE, const int FORANK, const int commutativity_groups[ARITY] = nullptr>
struct closure_base_t {

    typedef void(*ftype)(FITYPE, FOTYPE);
    typedef FITYPE fitype;
    typedef FOTYPE fotype;

    static constexpr const int arity = ARITY;
    static constexpr const int input_rank = FIRANK;
    static constexpr const int output_rank = FORANK;
    static constexpr const int* commutativity = commutativity_groups;

    static constexpr const void(*function)(nested_array_t<FITYPE, FIRANK>, nested_array_t<FOTYPE, FORANK>) =
        [](nested_array_t<FITYPE, FIRANK> iarray_in, nested_array_t<FOTYPE, FORANK> oarray_in) -> const void {
            return;
        };
};

/** Compile-time available container for metadata about functions. */
template<const int ARITY, typename FITYPE, const int FIRANK, const char IFNAME[], const char IVNAME[],
                          typename FOTYPE, const int FORANK, const char OFNAME[], const char OVNAME[], const int commutativity_groups[ARITY] = nullptr>
struct closure_nc_base_t {

    typedef void(*ftype)(FITYPE, FOTYPE);
    typedef FITYPE fitype;
    typedef FOTYPE fotype;

    static constexpr const int arity = ARITY;
    static constexpr const int input_rank = FIRANK;
    static constexpr const int output_rank = FORANK;
    static constexpr const int* commutativity = commutativity_groups;

    static constexpr const void(*function)(nested_netcdf_array_t<FITYPE, FIRANK, IFNAME, IVNAME>, nested_netcdf_array_t<FOTYPE, FORANK, OFNAME, OVNAME>) =
        [](nested_netcdf_array_t<FITYPE, FIRANK, IFNAME, IVNAME> iarray_in, nested_netcdf_array_t<FOTYPE, FORANK, OFNAME, OVNAME> oarray_in) -> const void {
            return;
        };

    /** This function is where the remaining output dimensions must be written. */
    template<const int IRANK, const int ORANK>
    static constexpr const void(*write_output_dims)(nested_netcdf_array_t<FITYPE, IRANK, IFNAME, IVNAME>, nested_netcdf_array_t<FOTYPE, ORANK, OFNAME, OVNAME>) =
        [](nested_netcdf_array_t<FITYPE, IRANK, IFNAME, IVNAME> iarray_in, nested_netcdf_array_t<FOTYPE, ORANK, OFNAME, OVNAME> oarray_in) -> const void {
            return;
        };
};

/** Compile-time available container for metadata about unary functions. */
template<typename FITYPE, const int FIRANK, typename FOTYPE, const int FORANK>
struct closure_base_unary_t : closure_base_t<1, FITYPE, FIRANK, FOTYPE, FORANK>{};

/** Compile-time available container for metadata about unary functions. */
template<typename FITYPE, const int FIRANK, const char IFNAME[], const char IVNAME[],
         typename FOTYPE, const int FORANK, const char OFNAME[], const char OVNAME[]>
struct closure_nc_base_unary_t : closure_nc_base_t<1, FITYPE, FIRANK, IFNAME, IVNAME,
                                                      FOTYPE, FORANK, OFNAME, OVNAME>{};

#endif
