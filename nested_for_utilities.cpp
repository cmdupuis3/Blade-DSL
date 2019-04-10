#ifndef NESTED_FOR_UTILITIES_CPP
#define NESTED_FOR_UTILITIES_CPP


#include <functional>
#include <type_traits>

#include "nested_array_utilities.cpp"
#include "nested_array.cpp"

using std::add_pointer;
using std::remove_pointer;

// Helper functions (must be constexpr)

typedef enum symcom_type {
    USE_NEITHER,
    USE_SYMMETRY,
    USE_COMMUTATIVITY,
    USE_BOTH_SYMMETRY_FIRST,
    USE_BOTH_COMMUTATIVITY_FIRST
} symcom_t;

namespace nested_for_utilities {

    template<typename TUPLE, typename TYPE, const int RANK, const int INDEX = std::tuple_size<TUPLE>::value - 1>
    constexpr bool is_same_recursive() {
        if constexpr (std::tuple_element<INDEX, TUPLE>::type::rank == RANK){
            if constexpr (INDEX == 0){
                return true;
            }else{
                return is_same_recursive<TUPLE, TYPE, RANK, INDEX - 1>();
            }
        } else {
            return false;
        }
    }

    template<typename TOP_TYPE, typename BOTTOM_TYPE>
    constexpr int max_depth(){

        if constexpr (std::is_same<TOP_TYPE, BOTTOM_TYPE>::value){
            return 0;
        } else {
            return 1 + max_depth<typename remove_pointer<TOP_TYPE>::type, BOTTOM_TYPE>();
        }

    }

    template<const int* vec, const int max, const int i = 0>
    constexpr int num_same(){

        if constexpr (max > 0  &&
                      i != max &&
                      vec[i] == vec [i + 1]) {
            return 1 + num_same<vec, max, i + 1>();
        } else {
            return 0;
        }

    }

    /** Find the number of symmetric dimensions for a set of commutative arguments */
    template<typename TUITYPE, const int* commutativity, const int arg_base, const int arg = arg_base>
    constexpr int num_symmetric_in_com_group(){

        constexpr const int nsym = num_same<std::tuple_element<arg, TUITYPE>::symmetry, std::tuple_size<TUITYPE>::value>();

        if constexpr (std::tuple_size<TUITYPE>::value > 1    &&
                      arg < std::tuple_size<TUITYPE>::value - 1 &&
                      commutativity[arg_base] == commutativity[arg + 1]) {
            return nsym + num_symmetric_in_com_group<TUITYPE, commutativity, arg_base, arg + 1>();
        } else {
            return nsym;
        }

    }

    /** Guess the best indexing strategy at compile time. */
    template<typename TUITYPE, const int ARG, const int* ICOM, const int DEPTH = 0>
    constexpr symcom_t guess_index_strategy(){

        if constexpr (ICOM && ARG < std::tuple_size<TUITYPE>::value - 1 && ICOM[ARG] == ICOM[ARG+1]){
            // Can use commutativity
            if constexpr (std::tuple_element<ARG, TUITYPE>::symmetry && std::tuple_element<ARG, TUITYPE>::symmetry[DEPTH] == std::tuple_element<ARG, TUITYPE>::symmetry[DEPTH + 1]) {
                // Can use both
                if constexpr (num_same<ICOM, std::tuple_size<TUITYPE>::value>() >
                              num_symmetric_in_com_group<TUITYPE, ICOM, ARG>){
                    // Commutativity is the better option
                    return USE_BOTH_COMMUTATIVITY_FIRST;
                }else{
                    // Symmetry is the better option
                    return USE_BOTH_SYMMETRY_FIRST;
                }
            }else{
                // Can only use commutativity
                return USE_COMMUTATIVITY;
            }
        } else if constexpr (std::tuple_element<ARG, TUITYPE>::symmetry && std::tuple_element<ARG, TUITYPE>::symmetry[DEPTH] == std::tuple_element<ARG, TUITYPE>::symmetry[DEPTH + 1]) {
            // Can use symmetry
            return USE_SYMMETRY;
        } else {
            return USE_NEITHER;
        }

    }

    /** Create a local copy of the current index table. Allows thread-safe commutativity. */
    template<typename TUITYPE, const int FIRANK, const int ARG, const int DEPTH>
    int** copy_index_table(TUITYPE iarray_in, int** index_table){
        using namespace nested_array_utilities;

        int** index_table_out = new int*[std::tuple_size<TUITYPE>::value];
        for(int j = 0; j < ARG; j++){
            index_table_out[j] = new int[std::get<j>(iarray_in).rank() - FIRANK - 1];
            for (int k = 0; k < std::get<j>(iarray_in).rank() - FIRANK - 1; k++){
                index_table_out[j][k] = index_table[j][k];
            }
        }
        for (int k = 0; k < DEPTH; k++){
            index_table_out[ARG][k] = index_table[ARG][k];
        }

        return index_table_out;
    }

    /** Index down into one array in a tuple of arrays and return the result */
    template<typename TUPLE, int INDEX, std::size_t... I>
    constexpr auto tuple_indexer(TUPLE arrays, int element){
        if constexpr (std::tuple_size<TUPLE>::value == 1){
            return [&arrays, element](){
                constexpr auto arg = std::make_tuple(std::get<0>(arrays)(element));
                return arg;
            };
        }else{

            return [&arrays, element](){
                constexpr auto arg = std::make_tuple(std::get<0>(arrays), tuple_indexer<TUPLE, INDEX+1>(std::get<I+1>(arrays)..., element));
                return arg;
            };
        }

    }

}

#endif
