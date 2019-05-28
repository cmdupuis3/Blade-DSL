
#ifndef METHOD_FOR_CHAINED_CPP
#define METHOD_FOR_CHAINED_CPP

#include <functional>
#include <type_traits>

#include "nested_array.cpp"
#include "nested_for_utilities.cpp"
#include "curry.cpp"

using std::add_pointer;
using std::remove_pointer;

using namespace nested_for_utilities;

//-----------------------------------------------------------------------//
//
// To any who dare tread here: I recommend running away, flailing wildly,
// and/or hysterical screaming. If you are not afraid, you should be.
//
//-----------------------------------------------------------------------//

template<const int ARITY, typename TUITYPE, const int ARG,   const int FIRANK, const int* ICOM,
                          typename OTYPE,   const int ORANK, const int* OSYM,  const int FORANK, const int DEPTH = 0>
class method_for_chained_base_t {

    typedef typename std::tuple_element<ARG, TUITYPE>::type::type ITYPE;
    static constexpr const int IRANK =  std::tuple_element<ARG, TUITYPE>::type::rank;
    typedef typename std::tuple_element<ARG, TUITYPE>::type::symmetry ISYM;
    typedef void (FTYPE)  (        vector< nested_array_t<ITYPE, FIRANK> >, nested_array_t<OTYPE, FORANK>);
    typedef void (MFTYPE) (size_t, vector< nested_array_t<ITYPE, FIRANK> >, nested_array_t<OTYPE, FORANK>);

    std::function<void(FTYPE)> loop;

public:
    method_for_chained_base_t(TUITYPE iarray_in, nested_array_t<OTYPE, ORANK, OSYM> oarray_in, int** index_table = nullptr, const int imin_in = 0);
    ~method_for_chained_base_t(){};

    void operator()(FTYPE  func_in);
    void operator()(MFTYPE multifunc_in);

    typename std::tuple_element<ARG, TUITYPE>::type* iarray = nullptr;
    nested_array_t<OTYPE, ORANK, OSYM>* oarray = nullptr;

};

template<const int ARITY, typename TUITYPE, const int ARG,   const int FIRANK, const int* ICOM,
                          typename OTYPE,   const int ORANK, const int* OSYM,  const int FORANK, const int DEPTH>
method_for_chained_base_t<ARITY, TUITYPE, ARG, FIRANK, ICOM, OTYPE, ORANK, OSYM, FORANK, DEPTH>::method_for_chained_base_t(TUITYPE iarray_in, nested_array_t<OTYPE, ORANK, OSYM> oarray_in, int** index_table, const int imin_in){

    // Note to devs: all 'if' statements need to be constexpr here to preserve performance.

    if constexpr (DEPTH == 0) {
        if constexpr (ARG == 0) {
            index_table = new int*[std::tuple_size<TUITYPE>::value];
        }
        index_table[ARG] = new int[std::tuple_element<ARG, TUITYPE>::type::rank - FIRANK];
    }

    // Are we done appending loops?
    if constexpr (is_same_recursive<TUITYPE, ITYPE, FIRANK>()){

        vector<nested_array_t<ITYPE, FIRANK> > iarrays_out;
        for(int i = 0; i < std::tuple_size<TUITYPE>::value; i++){
            iarrays_out.push_back(std::get<i>(iarray_in));
        }

        this->loop = [&iarrays_out, &oarray_in](FTYPE func){
                func(iarrays_out, oarray_in);
        };

    }else{

        // Get the indexing strategy for this argument
        constexpr const symcom_t symcom = guess_index_strategy<TUITYPE, ARG, ICOM, DEPTH>();

        // Can we use commutativity to index here?
        if constexpr (symcom == USE_COMMUTATIVITY && ARG != 0){
            this->loop = [&iarray_in, &oarray_in, &index_table, imin_in](FTYPE func){
                for(int i = index_table[ARG-1][DEPTH]; i < iarray_in.current_extent(); i++){
                    int** index_table_local = copy_index_table<TUITYPE, FIRANK, ARG, DEPTH>(iarray_in, index_table);
                    index_table_local[ARG][DEPTH] = i;
                    if constexpr (ARG == std::tuple_size<TUITYPE>::value - 1){
                        if constexpr (ORANK == FORANK){
                            method_for_chained_base_t<ARITY, TUITYPE, ARG + 1, FIRANK, ICOM, OTYPE, ORANK, OSYM, FORANK, DEPTH
                                > next(tuple_indexer<TUITYPE, ARG>(iarray_in, i), oarray_in, index_table_local, i);
                            next(func);
                        } else {
                            method_for_chained_base_t<ARITY, TUITYPE, ARG + 1, FIRANK, ICOM, OTYPE, ORANK, OSYM, FORANK, DEPTH
                                > next(tuple_indexer<TUITYPE, ARG>(iarray_in, i), oarray_in(i), index_table_local, i);
                            next(func);
                        }
                    } else {
                        if constexpr (ORANK == FORANK){
                            method_for_chained_base_t<ARITY, TUITYPE, ARG, FIRANK, ICOM, OTYPE, ORANK, OSYM, FORANK, DEPTH + 1
                                > next(tuple_indexer<TUITYPE, ARG>(iarray_in, i), oarray_in, index_table_local, i);
                            next(func);
                        } else {
                            method_for_chained_base_t<ARITY, TUITYPE, ARG, FIRANK, ICOM, OTYPE, ORANK, OSYM, FORANK, DEPTH + 1
                                > next(tuple_indexer<TUITYPE, ARG>(iarray_in, i), oarray_in(i), index_table_local, i);
                            next(func);
                        }
                    }
                }
            };

        // Can we use symmetry as well?
        } else if constexpr (symcom == USE_BOTH_COMMUTATIVITY_FIRST && ARG != 0){
            this->loop = [&iarray_in, &oarray_in, &index_table, imin_in](FTYPE func){
                int imin = index_table[ARG-1][DEPTH] > imin_in ? index_table[ARG-1][DEPTH] : imin_in;
                for(int i = imin; i < iarray_in.current_extent(); i++){
                    int** index_table_local = copy_index_table<TUITYPE, FIRANK, ARG, DEPTH>(iarray_in, index_table);
                    index_table_local[ARG][DEPTH] = i;
                    if constexpr (ARG == std::tuple_size<TUITYPE>::value - 1){
                        if constexpr (ORANK == FORANK){
                            method_for_chained_base_t<ARITY, TUITYPE, ARG + 1, FIRANK, ICOM, OTYPE, ORANK, OSYM, FORANK, DEPTH
                                > next(tuple_indexer<TUITYPE, ARG>(iarray_in, i), oarray_in, index_table_local, i);
                            next(func);
                        } else {
                            method_for_chained_base_t<ARITY, TUITYPE, ARG + 1, FIRANK, ICOM, OTYPE, ORANK, OSYM, FORANK, DEPTH
                                > next(tuple_indexer<TUITYPE, ARG>(iarray_in, i), oarray_in(i), index_table_local, i);
                            next(func);
                        }
                    } else {
                        if constexpr (ORANK == FORANK){
                            method_for_chained_base_t<ARITY, TUITYPE, ARG, FIRANK, ICOM, OTYPE, ORANK, OSYM, FORANK, DEPTH + 1
                                > next(tuple_indexer<TUITYPE, ARG>(iarray_in, i), oarray_in, index_table_local, i);
                            next(func);
                        } else {
                            method_for_chained_base_t<ARITY, TUITYPE, ARG, FIRANK, ICOM, OTYPE, ORANK, OSYM, FORANK, DEPTH + 1
                                > next(tuple_indexer<TUITYPE, ARG>(iarray_in, i), oarray_in(i), index_table_local, i);
                            next(func);
                        }
                    }
                }
            };

        // Can we at least use symmetry to index?
        } else if constexpr (symcom == USE_SYMMETRY || symcom == USE_BOTH_SYMMETRY_FIRST ||
                            (symcom == USE_BOTH_COMMUTATIVITY_FIRST && ARG == 0)){
            this->loop = [&iarray_in, &oarray_in, &index_table, imin_in](FTYPE func){
                for(int i = imin_in; i < iarray_in.current_extent(); i++){
                    int** index_table_local = copy_index_table<TUITYPE, FIRANK, ARG, DEPTH>(iarray_in, index_table);
                    index_table_local[ARG][DEPTH] = i;
                    if constexpr (ARG == std::tuple_size<TUITYPE>::value - 1){
                        if constexpr (ORANK == FORANK){
                            method_for_chained_base_t<ARITY, TUITYPE, ARG + 1, FIRANK, ICOM, OTYPE, ORANK, OSYM, FORANK, DEPTH
                                > next(tuple_indexer<TUITYPE, ARG>(iarray_in, i), oarray_in, index_table_local, i);
                            next(func);
                        } else {
                            method_for_chained_base_t<ARITY, TUITYPE, ARG + 1, FIRANK, ICOM, OTYPE, ORANK, OSYM, FORANK, DEPTH
                                > next(tuple_indexer<TUITYPE, ARG>(iarray_in, i), oarray_in(i), index_table_local, i);
                            next(func);
                        }
                    } else {
                        if constexpr (ORANK == FORANK){
                            method_for_chained_base_t<ARITY, TUITYPE, ARG, FIRANK, ICOM, OTYPE, ORANK, OSYM, FORANK, DEPTH + 1
                                > next(tuple_indexer<TUITYPE, ARG>(iarray_in, i), oarray_in, index_table_local, i);
                            next(func);
                        } else {
                            method_for_chained_base_t<ARITY, TUITYPE, ARG, FIRANK, ICOM, OTYPE, ORANK, OSYM, FORANK, DEPTH + 1
                                > next(tuple_indexer<TUITYPE, ARG>(iarray_in, i), oarray_in(i), index_table_local, i);
                            next(func);
                        }
                    }
                }
            };

        // There is no way to optimize the indexing strategy, so we're doing it the normal way.
        }else{
            this->loop = [&iarray_in, &oarray_in, &index_table, imin_in](FTYPE func){
                for(int i = imin_in; i < iarray_in.current_extent(); i++){
                    int** index_table_local = copy_index_table<TUITYPE, FIRANK, ARG, DEPTH>(iarray_in, index_table);
                    index_table_local[ARG][DEPTH] = i;
                    if constexpr (ARG == std::tuple_size<TUITYPE>::value - 1){
                        if constexpr (ORANK == FORANK){
                            method_for_chained_base_t<ARITY, TUITYPE, ARG + 1, FIRANK, ICOM, OTYPE, ORANK, OSYM, FORANK, DEPTH
                                > next(tuple_indexer<TUITYPE, ARG>(iarray_in, i), oarray_in, index_table_local);
                            next(func);
                        } else {
                            method_for_chained_base_t<ARITY, TUITYPE, ARG + 1, FIRANK, ICOM, OTYPE, ORANK, OSYM, FORANK, DEPTH
                                > next(tuple_indexer<TUITYPE, ARG>(iarray_in, i), oarray_in(i), index_table_local);
                            next(func);
                        }
                    } else {
                        if constexpr (ORANK == FORANK){
                            method_for_chained_base_t<ARITY, TUITYPE, ARG, FIRANK, ICOM, OTYPE, ORANK, OSYM, FORANK, DEPTH + 1
                                > next(tuple_indexer<TUITYPE, ARG>(iarray_in, i), oarray_in, index_table_local);
                            next(func);
                        } else {
                            method_for_chained_base_t<ARITY, TUITYPE, ARG, FIRANK, ICOM, OTYPE, ORANK, OSYM, FORANK, DEPTH + 1
                                > next(tuple_indexer<TUITYPE, ARG>(iarray_in, i), oarray_in(i), index_table_local);
                            next(func);
                        }
                    }
                }
            };
        }

    }

}

template<const int ARITY, typename TUITYPE, const int ARG,   const int FIRANK, const int* ICOM,
                          typename OTYPE,   const int ORANK, const int* OSYM,  const int FORANK, const int DEPTH>
void method_for_chained_base_t<ARITY, TUITYPE, ARG, FIRANK, ICOM, OTYPE, ORANK, OSYM, FORANK, DEPTH>::operator()(FTYPE func_in){

    // Are we done appending loops?
    if constexpr (IRANK != FIRANK){
        this->loop(func_in);
    }else{
        func_in(*(this->iarray), *(this->oarray));
    }

    return;
}

template<const int ARITY, typename TUITYPE, const int ARG,   const int FIRANK, const int* ICOM,
                          typename OTYPE,   const int ORANK, const int* OSYM,  const int FORANK, const int DEPTH>
void method_for_chained_base_t<ARITY, TUITYPE, ARG, FIRANK, ICOM, OTYPE, ORANK, OSYM, FORANK, DEPTH>::operator()(MFTYPE multifunc_in){
    this->loop(curry(1, multifunc_in));
    return;
}

template<typename TUITYPE, typename NOTYPE, typename CLTYPE>
class method_for_chained_t : public method_for_chained_base_t<CLTYPE::arity, TUITYPE, 0,   CLTYPE::input_rank, CLTYPE::commutativity,
                                                                    typename NOTYPE::type, NOTYPE::rank,       NOTYPE::symmetry,      CLTYPE::output_rank>{
                      public: using method_for_chained_base_t<CLTYPE::arity, TUITYPE, 0,   CLTYPE::input_rank, CLTYPE::commutativity,
                                                                    typename NOTYPE::type, NOTYPE::rank,       NOTYPE::symmetry,      CLTYPE::output_rank>::method_for_chained_base_t;
};

#endif
