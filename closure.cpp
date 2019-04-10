#ifndef CLOSURE_CPP
#define CLOSURE_CPP

#include <functional>
#include <type_traits>
#include <vector>

using std::add_pointer;
using std::remove_pointer;
using std::vector;

#include "nested_array.cpp"
#include "nested_array_utilities.cpp"
#include "curry.cpp"


template<const int ARITY, typename FITYPE, const int FIRANK, typename FOTYPE, const int FORANK, const int commutativity_groups[ARITY] = nullptr>
struct closure_base_t {

    typedef void(*ftype)(FITYPE, FOTYPE);
    typedef FITYPE fitype;
    typedef FOTYPE fotype;

    static constexpr const int arity = ARITY;
    static constexpr const int input_rank = FIRANK;
    static constexpr const int output_rank = FORANK;
    static constexpr const int* commutativity = commutativity_groups;

};

template<typename FITYPE, const int FIRANK, typename FOTYPE, const int FORANK>
struct closure_base_unary_t : closure_base_t<1, FITYPE, FIRANK, FOTYPE, FORANK>{};

#endif
