

#ifndef METHOD_FOR_CHAINED_CPP
#define METHOD_FOR_CHAINED_CPP

#include <functional>
#include <type_traits>

#include "nested_array.cpp"
#include "nested_for_utilities.cpp"
#include "curry.cpp"
#include "closure.cpp"

using std::add_pointer;
using std::remove_pointer;

//-----------------------------------------------------------------------//
//
// To any who dare tread here: I recommend running away, flailing wildly,
// and/or hysterical screaming. If you are not afraid, you should be.
//
//-----------------------------------------------------------------------//

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


template<typename TUITYPE, const int arg, const int* ICOM, typename OTYPE, typename FITYPE, typename FOTYPE, const int depth = 0>
class method_for_chained_base_t {

    typedef typename std::tuple_element<arg, TUITYPE>::type::type ITYPE;
    typedef typename std::tuple_element<arg, TUITYPE>::type::symmetry ISYM;
    typedef void (FTYPE)  (        vector< nested_array_t<FITYPE> >, nested_array_t<FOTYPE>);
    typedef void (MFTYPE) (size_t, vector< nested_array_t<FITYPE> >, nested_array_t<FOTYPE>);

    std::function<void(FTYPE)> loop;

public:
    method_for_chained_base_t(TUITYPE iarray_in, nested_array_t<OTYPE> oarray_in, int** index_table = nullptr, const int imin_in = 0);
    ~method_for_chained_base_t(){};

    void operator()(FTYPE  func_in);
    void operator()(MFTYPE multifunc_in);

    typename std::tuple_element<arg, TUITYPE>::type* iarray = nullptr;
    nested_array_t<OTYPE>* oarray = nullptr;

};

template<typename TUITYPE, const int arg, const int* ICOM, typename OTYPE, typename FITYPE, typename FOTYPE, const int depth>
method_for_chained_base_t<TUITYPE, arg, ICOM, OTYPE, FITYPE, FOTYPE, depth>::method_for_chained_base_t(TUITYPE iarray_in, nested_array_t<OTYPE> oarray_in, int** index_table, const int imin_in){

    // Note to devs: all 'if' statements need to be constexpr here to preserve performance.

    if constexpr (depth == 0) {
        if constexpr (arg == 0) {
            index_table = new int*[num_same<ICOM, std::tuple_size<TUITYPE>::value>()];
        }
        index_table[arg] = new int[max_depth<std::tuple_element<arg, TUITYPE>::type::type, FITYPE>() - 1];
    }

    // Are we done appending loops?
    if constexpr (is_same_recursive<TUITYPE, std::tuple_size<TUITYPE>::value, FITYPE>()){

        vector<nested_array_t<FITYPE>> iarrays_out;
        for(int i = 0; i < std::tuple_size<TUITYPE>::value; i++){
            iarrays_out.push_back(std::get<i>(iarray_in));
        }

        this->loop = [&iarrays_out, &oarray_in](FTYPE func){
                func(iarrays_out, oarray_in);
        };

    }else{

        // Get the indexing strategy for this argument
        constexpr const symcom_t symcom = index_strategy<TUITYPE, arg, ICOM, OTYPE, FITYPE, FOTYPE, depth>();

        // Can we only use commutativity to index here?
        if constexpr (symcom == USE_COMMUTATIVITY && arg != 0){
            this->loop = [&iarray_in, &oarray_in, &index_table, imin_in](FTYPE func){
                for(int i = index_table[arg-1][depth]; i < iarray_in.current_extent(); i++){
                    int** index_table_local = copy_index_table<TUITYPE, FITYPE, arg, depth>(iarray_in, index_table);
                    index_table_local[arg][depth] = i;
                    if constexpr (arg == max_depth<std::tuple_element<arg, TUITYPE>::type::type, FITYPE>() - 1){
                        if constexpr (std::is_same<OTYPE, FOTYPE>::value){
                            method_for_chained_base_t<TUITYPE, arg + 1, ICOM, OTYPE, FITYPE, FOTYPE, 0
                                > next(tuple_indexer<TUITYPE, arg>(iarray_in, i), oarray_in, index_table_local, i);
                            next(func);
                        } else {
                            method_for_chained_base_t<TUITYPE, arg + 1, ICOM, OTYPE, FITYPE, FOTYPE, 0
                                > next(tuple_indexer<TUITYPE, arg>(iarray_in, i), oarray_in(i), index_table_local, i);
                            next(func);
                        }
                    } else {
                        if constexpr (std::is_same<OTYPE, FOTYPE>::value){
                            method_for_chained_base_t<TUITYPE, arg, ICOM, OTYPE, FITYPE, FOTYPE, depth + 1
                                > next(tuple_indexer<TUITYPE, arg>(iarray_in, i), oarray_in, index_table_local, i);
                            next(func);
                        } else {
                            method_for_chained_base_t<TUITYPE, arg, ICOM, OTYPE, FITYPE, FOTYPE, depth + 1
                                > next(tuple_indexer<TUITYPE, arg>(iarray_in, i), oarray_in(i), index_table_local, i);
                            next(func);
                        }
                    }
                }
            };

        // Can we use commutativity and symmetry here?
        } else if constexpr (symcom == USE_BOTH_COMMUTATIVITY_FIRST && arg != 0){
            this->loop = [&iarray_in, &oarray_in, &index_table, imin_in](FTYPE func){
                int imin = index_table[arg-1][depth] > imin_in ? index_table[arg-1][depth] : imin_in;
                for(int i = imin; i < iarray_in.current_extent(); i++){
                    int** index_table_local = copy_index_table<TUITYPE, FITYPE, arg, depth>(iarray_in, index_table);
                    index_table_local[arg][depth] = i;
                    if constexpr (arg == max_depth<std::tuple_element<arg, TUITYPE>::type::type, FITYPE>() - 1){
                        if constexpr (std::is_same<OTYPE, FOTYPE>::value){
                            method_for_chained_base_t<TUITYPE, arg + 1, ICOM, OTYPE, FITYPE, FOTYPE, 0
                                > next(tuple_indexer<TUITYPE, arg>(iarray_in, i), oarray_in, index_table_local, i);
                            next(func);
                        } else {
                            method_for_chained_base_t<TUITYPE, arg + 1, ICOM, OTYPE, FITYPE, FOTYPE, 0
                                > next(tuple_indexer<TUITYPE, arg>(iarray_in, i), oarray_in(i), index_table_local, i);
                            next(func);
                        }
                    } else {
                        if constexpr (std::is_same<OTYPE, FOTYPE>::value){
                            method_for_chained_base_t<TUITYPE, arg, ICOM, OTYPE, FITYPE, FOTYPE, depth + 1
                                > next(tuple_indexer<TUITYPE, arg>(iarray_in, i), oarray_in, index_table_local, i);
                            next(func);
                        } else {
                            method_for_chained_base_t<TUITYPE, arg, ICOM, OTYPE, FITYPE, FOTYPE, depth + 1
                                > next(tuple_indexer<TUITYPE, arg>(iarray_in, i), oarray_in(i), index_table_local, i);
                            next(func);
                        }
                    }
                }
            };

        // Can we at least use symmetry to index?
        } else if constexpr (symcom == USE_SYMMETRY || symcom == USE_BOTH_SYMMETRY_FIRST ||
                            (symcom == USE_BOTH_COMMUTATIVITY_FIRST && arg == 0)){
            this->loop = [&iarray_in, &oarray_in, &index_table, imin_in](FTYPE func){
                for(int i = imin_in; i < iarray_in.current_extent(); i++){
                    int** index_table_local = copy_index_table<TUITYPE, FITYPE, arg, depth>(iarray_in, index_table);
                    index_table_local[arg][depth] = i;
                    if constexpr (arg == max_depth<std::tuple_element<arg, TUITYPE>::type::type, FITYPE>() - 1){
                        if constexpr (std::is_same<OTYPE, FOTYPE>::value){
                            method_for_chained_base_t<TUITYPE, arg + 1, ICOM, OTYPE, FITYPE, FOTYPE, 0
                                > next(tuple_indexer<TUITYPE, arg>(iarray_in, i), oarray_in, index_table_local, i);
                            next(func);
                        } else {
                            method_for_chained_base_t<TUITYPE, arg + 1, ICOM, OTYPE, FITYPE, FOTYPE, 0
                                > next(tuple_indexer<TUITYPE, arg>(iarray_in, i), oarray_in(i), index_table_local, i);
                            next(func);
                        }
                    } else {
                        if constexpr (std::is_same<OTYPE, FOTYPE>::value){
                            method_for_chained_base_t<TUITYPE, arg, ICOM, OTYPE, FITYPE, FOTYPE, depth + 1
                                > next(tuple_indexer<TUITYPE, arg>(iarray_in, i), oarray_in, index_table_local, i);
                            next(func);
                        } else {
                            method_for_chained_base_t<TUITYPE, arg, ICOM, OTYPE, FITYPE, FOTYPE, depth + 1
                                > next(tuple_indexer<TUITYPE, arg>(iarray_in, i), oarray_in(i), index_table_local, i);
                            next(func);
                        }
                    }
                }
            };

        // There is no way to optimize the indexing strategy, so we're doing it the normal way.
        }else{
            this->loop = [&iarray_in, &oarray_in, &index_table, imin_in](FTYPE func){
                for(int i = imin_in; i < iarray_in.current_extent(); i++){
                    int** index_table_local = copy_index_table<TUITYPE, FITYPE, arg, depth>(iarray_in, index_table);
                    index_table_local[arg][depth] = i;
                    if constexpr (arg == max_depth<std::tuple_element<arg, TUITYPE>::type::type, FITYPE>() - 1){
                        if constexpr (std::is_same<OTYPE, FOTYPE>::value){
                            method_for_chained_base_t<TUITYPE, arg + 1, ICOM, OTYPE, FITYPE, FOTYPE, 0
                                > next(tuple_indexer<TUITYPE, arg>(iarray_in, i), oarray_in, index_table_local);
                            next(func);
                        } else {
                            method_for_chained_base_t<TUITYPE, arg + 1, ICOM, OTYPE, FITYPE, FOTYPE, 0
                                > next(tuple_indexer<TUITYPE, arg>(iarray_in, i), oarray_in(i), index_table_local);
                            next(func);
                        }
                    } else {
                        if constexpr (std::is_same<OTYPE, FOTYPE>::value){
                            method_for_chained_base_t<TUITYPE, arg, ICOM, OTYPE, FITYPE, FOTYPE, depth + 1
                                > next(tuple_indexer<TUITYPE, arg>(iarray_in, i), oarray_in, index_table_local);
                            next(func);
                        } else {
                            method_for_chained_base_t<TUITYPE, arg, ICOM, OTYPE, FITYPE, FOTYPE, depth + 1
                                > next(tuple_indexer<TUITYPE, arg>(iarray_in, i), oarray_in(i), index_table_local);
                            next(func);
                        }
                    }
                }
            };
        }

    }

}

template<typename TUITYPE, const int arg, const int* ICOM, typename OTYPE, typename FITYPE, typename FOTYPE, const int depth>
void method_for_chained_base_t<TUITYPE, arg, ICOM, OTYPE, FITYPE, FOTYPE, depth>::operator()(FTYPE func_in){

    // Are we done appending loops?
    if constexpr (!std::is_same<ITYPE, FITYPE>::value){
        this->loop(func_in);
    }else{
        func_in(*(this->iarray), *(this->oarray));
    }

    return;
}

template<typename TUITYPE, const int arg, const int* ICOM, typename OTYPE, typename FITYPE, typename FOTYPE, const int depth>
void method_for_chained_base_t<TUITYPE, arg, ICOM, OTYPE, FITYPE, FOTYPE, depth>::operator()(MFTYPE multifunc_in){
    this->loop(curry(1, multifunc_in));
    return;
}

template<typename TUITYPE, typename OTYPE, typename CLTYPE>
class method_for_chained_t : public method_for_chained_base_t<TUITYPE, 0, CLTYPE::commutativity, OTYPE, typename CLTYPE::fitype, typename CLTYPE::fotype>{
                      public: using method_for_chained_base_t<TUITYPE, 0, CLTYPE::commutativity, OTYPE, typename CLTYPE::fitype, typename CLTYPE::fotype>::method_for_chained_base_t;
};

/*
template<typename TUITYPE, typename OTYPE, typename FITYPE, typename FOTYPE, const int arg = 0, const bool first = true>
class method_for_chained_t {

    //typedef typename std::tuple_element<0, TUITYPE>::type ITYPE;
    typedef std::function<void(vector<nested_array_t<FITYPE>>, nested_array_t<FOTYPE>)> FTYPE;

    std::function<void(FTYPE)> loop;
    vector<nested_array_t<FITYPE>>* iarrays_out;



public:
    method_for_chained_t(TUITYPE iarrays_in, OTYPE oarray_in);
    ~method_for_chained_t(){};

    void operator()(FTYPE func_in);

};

template<typename TUITYPE, typename OTYPE, typename FITYPE, typename FOTYPE, const int arg, const bool first>
method_for_chained_t<TUITYPE, OTYPE, FITYPE, FOTYPE, arg, first>::method_for_chained_t(TUITYPE iarrays_in, OTYPE oarray_in){

    if constexpr (first){
        vector<nested_array_t<FITYPE>>* iarrays_out = new vector<nested_array_t<FITYPE>>;
    }

    if constexpr (arg == std::tuple_size<TUITYPE>::value - 1) {

        this->loop = [&iarrays_in, &oarray_in, &iarrays_out](FTYPE func) {

        }

    }else{

        this->loop = [&iarrays_in, &oarray_in, &iarrays_out](FTYPE func) {

            auto last = method_for_t<typename std::tuple_element<arg, TUITYPE>::type, OTYPE, FITYPE, FOTYPE>(std::get<arg>(iarrays_in), oarray_in);
            method_for_chained_t<TUITYPE, OTYPE, FITYPE, FOTYPE, arg + 1> next(iarrays_in, oarray_in);
            last([&next, &func, &iarrays_out](nested_array_t<FITYPE> iarray, nested_array_t<FOTYPE> oarray){
                iarrays_out.push_back(iarray);
                next([&iarrays_out, &oarray](FTYPE func){
                    func(iarrays_out, oarray);
                });
            });

        };

    }

}
*/

#endif

