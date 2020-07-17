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

#ifndef NESTED_ARRAY_UTILITIES_CPP
#define NESTED_ARRAY_UTILITIES_CPP


#include <functional>
#include <type_traits>

using std::add_pointer;
using std::remove_pointer;
using std::is_pointer;


// Helper functions (must be constexpr)

namespace nested_array_utilities {

    /** Find the rank of a type */
    template<typename TYPE>
    constexpr const size_t get_rank() {

        if constexpr (std::is_pointer<TYPE>::value) {
            return 1 + get_rank<typename std::remove_pointer<TYPE>::type>();
        } else {
            return 0;
        }

    }

    /** promote class' implementation function */
    template<typename TYPE, const int rank, const int depth = 0>
    constexpr auto promote_impl() {

        if constexpr (depth < rank) {
            return promote_impl<typename add_pointer<TYPE>::type, rank, depth + 1>();
        } else if constexpr (depth == rank) {
            TYPE dummy = {0};
            return dummy;
        } else {
            return; // fatal
        }

    }

    /** Class to allow promotion of a value type by an arbitrary number of pointers at compile time. */
    template<typename TYPE, const int rank, const int depth = 0>
    class promote {

    public:
        promote(){};
        ~promote(){};

        typedef decltype(promote_impl<TYPE, rank>()) type;

    };

    template<typename TYPE, const int SYMM[], const int DEPTH = 0>
    constexpr void fold(TYPE array, const size_t extents[]) {

        typedef typename remove_pointer<TYPE>::type DTYPE;

        if constexpr (get_rank<TYPE>() > 1) {
            for (size_t i = 0; i < extents[DEPTH]; i++) {
                if constexpr (SYMM[DEPTH] == SYMM[DEPTH+1]) {
                    for (size_t j = 0; j < i; j++) {
                        array[i][j] = array[j][i];
                    }
                }
                fold<DTYPE, SYMM, DEPTH+1>(array[i], extents);
            }
        }

    }

    /** Recursively allocate an array, with dimensionality deduced from TYPE, according to arrays
     *  of index minima and maxima. Minima default to zero in every dimension.
     */
    template<typename TYPE, const int SYMM[] = nullptr, const int DEPTH = 0>
    constexpr TYPE allocate(const size_t extents[], const int lastIndex = 0) {

        typedef typename remove_pointer<TYPE>::type DTYPE;

        TYPE array = new DTYPE[extents[DEPTH]];
        if constexpr (get_rank<TYPE>() > 1) {
            for (int i = lastIndex; i < extents[DEPTH]; i++) {
                if constexpr (SYMM && SYMM[DEPTH] == SYMM[DEPTH+1]) {
                    array[i] = allocate<DTYPE, SYMM, DEPTH+1>(extents, i);
                } else {
                    array[i] = allocate<DTYPE, SYMM, DEPTH+1>(extents);
                }
            }
        }

        if constexpr (DEPTH == 0 && SYMM) {
            fold<TYPE, SYMM>(array, extents);
        }

        return array;

    }

    /** Recursively fill an array with random numbers, with dimensionality deduced from TYPE. */
    template<typename TYPE, const int SYMM[] = nullptr, const int DEPTH = 0>
    constexpr auto fill_random(TYPE array_in, const int extents[], int mod_in, int lastIndex = 0) {

        typedef typename remove_pointer<TYPE>::type DTYPE;

        if constexpr (std::is_pointer<DTYPE>::value) {
            for (int i = lastIndex; i < extents[DEPTH]; i++) {
                if constexpr (SYMM && SYMM[DEPTH] == SYMM[DEPTH+1]) {
                    fill_random<DTYPE, extents, SYMM, DEPTH+1>(array_in[i], mod_in, i);
                } else {
                    fill_random<DTYPE, extents, SYMM, DEPTH+1>(array_in[i], mod_in);
                }
            }
        } else {
            for (int i = lastIndex; i < extents[DEPTH]; i++) {
                array_in[i] = rand() % mod_in;
            }
        }

    }


}

#endif
