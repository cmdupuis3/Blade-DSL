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

#ifndef NESTED_ARRAY_CPP
#define NESTED_ARRAY_CPP

#include <iostream>
#include <type_traits>

using std::add_pointer;
using std::cout;
using std::endl;
using std::remove_pointer;

#include "nested_array_utilities.cpp"

using namespace nested_array_utilities;



template<typename VTYPE, const int rank_t, const int* symmetry_groups>
class nested_base_t {

protected:

    typedef typename promote<VTYPE, rank_t>::type TYPE;

    TYPE data;

    int c_depth;
    int index; // Current index

    int* extents; //< Length of dimensions; A value of -1 signifies a ragged dimension

public:
    nested_base_t(){};
    virtual ~nested_base_t(){};

    typedef TYPE type;
    typedef VTYPE value_type;
    static constexpr const int* symmetry = symmetry_groups;
    static constexpr const int rank = rank_t;

    auto get_data() const;
    constexpr int get_rank() const;
    int current_depth() const;
    int get_index() const;

    int current_extent() const;
    int next_extent() const;
    int extent(int) const;
    int* get_extents() const;

    bool current_raggedness() const;
    bool next_raggedness() const;
    bool raggedness(int) const;

    constexpr int current_symmetry_group() const;
    constexpr int next_symmetry_group() const;
    constexpr int symmetry_group(int) const;
};


/** \brief A class representing a hyperblock of data of arbitrary dimensionality.
 *         May be ragged in any or all dimensions.
 */
template<typename VTYPE, const int rank_t, const int* symmetry_groups = nullptr>
class nested_array_t : public nested_base_t<VTYPE, rank_t, symmetry_groups> {

private:
    typedef typename promote<VTYPE, rank_t>::type TYPE;
    typedef typename add_pointer<TYPE>::type UTYPE;
    typedef typename remove_pointer<TYPE>::type DTYPE;

protected:
    const nested_array_t<VTYPE, rank_t+1, symmetry_groups>* parent;

public:
    nested_array_t();
    nested_array_t(int*, TYPE, int index = 0);
    nested_array_t(const nested_array_t<VTYPE, rank_t+1, symmetry_groups>*, int index = 0); //< Downward traversal pointer-type constructor; not for public consumption
    ~nested_array_t();

    const nested_array_t<VTYPE, rank_t-1, symmetry_groups> operator()(int, int) const; //< alias for 'down', plus a dereference; ragged state
    const nested_array_t<VTYPE, rank_t-1, symmetry_groups> operator()(int) const;      //< alias for 'down', plus a dereference; non-ragged state
    DTYPE&                operator[](int);
    const DTYPE&          operator[](int) const;

    void                  operator=(TYPE); //< Shallow-copy data into this array
    void                  operator=(nested_array_t<VTYPE, rank_t, symmetry_groups>);
    nested_array_t<VTYPE, rank_t,   symmetry_groups>  operator+(nested_array_t<VTYPE, rank_t, symmetry_groups>); //< merge along this dimension
    nested_array_t<VTYPE, rank_t,   symmetry_groups>* operator/(int);                    //< split along this dimension

    const nested_array_t<VTYPE, rank_t+1, symmetry_groups>* up() const;
    const nested_array_t<VTYPE, rank_t-1, symmetry_groups>* down(int, int) const;
    const nested_array_t<VTYPE, rank_t-1, symmetry_groups>* down(int) const;
    const nested_array_t<VTYPE, rank_t,   symmetry_groups>* next() const;
    const nested_array_t<VTYPE, rank_t,   symmetry_groups>* first() const;
};

//template<typename TYPE>
//class nested_array_asym_t : public nested_array_t<TYPE, nullptr>{ public: using nested_array_t<TYPE,nullptr>::nested_array_t; };

//-----------------------------------------------------//
//
//                Base methods:
//
//-----------------------------------------------------//

template<typename VTYPE, const int rank_t, const int* symmetry_groups>
auto nested_base_t<VTYPE, rank_t, symmetry_groups>::get_data() const {
    return this->data; // unsafe!
}
template<typename VTYPE, const int rank_t, const int* symmetry_groups>
constexpr int nested_base_t<VTYPE, rank_t, symmetry_groups>::get_rank() const {
    return rank_t;
}
template<typename VTYPE, const int rank_t, const int* symmetry_groups>
int nested_base_t<VTYPE, rank_t, symmetry_groups>::current_depth() const {
    return this->c_depth;
}
template<typename VTYPE, const int rank_t, const int* symmetry_groups>
int nested_base_t<VTYPE, rank_t, symmetry_groups>::get_index() const {
    return this->index;
}
template<typename VTYPE, const int rank_t, const int* symmetry_groups>
int nested_base_t<VTYPE, rank_t, symmetry_groups>::current_extent() const {
    return this->extents[this->c_depth];
}
template<typename VTYPE, const int rank_t, const int* symmetry_groups>
int nested_base_t<VTYPE, rank_t, symmetry_groups>::next_extent() const {
    return this->extents[this->c_depth + 1];
}
template<typename VTYPE, const int rank_t, const int* symmetry_groups>
int nested_base_t<VTYPE, rank_t, symmetry_groups>::extent(int depth_in) const {
    return this->extents[depth_in];
}
template<typename VTYPE, const int rank_t, const int* symmetry_groups>
int* nested_base_t<VTYPE, rank_t, symmetry_groups>::get_extents() const {
    return this->extents;
}
template<typename VTYPE, const int rank_t, const int* symmetry_groups>
bool nested_base_t<VTYPE, rank_t, symmetry_groups>::current_raggedness() const {
    return (this->extents[this->c_depth] == -1) ? true : false;
}
template<typename VTYPE, const int rank_t, const int* symmetry_groups>
bool nested_base_t<VTYPE, rank_t, symmetry_groups>::next_raggedness() const {
    return (this->extents[this->c_depth + 1] == -1) ? true : false;
}
template<typename VTYPE, const int rank_t, const int* symmetry_groups>
bool nested_base_t<VTYPE, rank_t, symmetry_groups>::raggedness(int depth_in) const {
    return (this->extents[depth_in] == -1) ? true : false;
}
template<typename VTYPE, const int rank_t, const int* symmetry_groups>
constexpr int nested_base_t<VTYPE, rank_t, symmetry_groups>::current_symmetry_group() const {
    return symmetry_groups ? symmetry_groups[this->c_depth] : this->c_depth;
}
template<typename VTYPE, const int rank_t, const int* symmetry_groups>
constexpr int nested_base_t<VTYPE, rank_t, symmetry_groups>::next_symmetry_group() const {
    return symmetry_groups ? symmetry_groups[this->c_depth + 1] : this->c_depth + 1;
}
template<typename VTYPE, const int rank_t, const int* symmetry_groups>
constexpr int nested_base_t<VTYPE, rank_t, symmetry_groups>::symmetry_group(int depth_in) const {
    return symmetry_groups ? symmetry_groups[depth_in] : depth_in;
}

template<typename VTYPE, const int rank_t, const int* symmetry_groups>
nested_array_t<VTYPE, rank_t, symmetry_groups>::nested_array_t(){

}

template<typename VTYPE, const int rank_t, const int* symmetry_groups>
nested_array_t<VTYPE, rank_t, symmetry_groups>::nested_array_t(int* extents_in, TYPE data_in, int index_in){

    this->data = data_in;

    this->c_depth = 0;
    this->index = index_in;

    /** Deep-copy extents */
    this->extents = new int[rank_t];
    for(int i = 0; i < rank_t; i++){
        this->extents[i] = extents_in[i];
    }

    this->parent = nullptr;
}


template<typename VTYPE, const int rank_t, const int* symmetry_groups>
void nested_array_t<VTYPE, rank_t, symmetry_groups>::operator=(nested_array_t<VTYPE, rank_t, symmetry_groups> array_in){

    this->data = array_in.get_data();

    this->c_depth = array_in.current_depth();
    this->index = array_in.get_index();

    /** Deep-copy extents */
    this->extents = new int[rank_t];
    for(int i = 0; i < rank_t; i++){
        this->extents[i] = array_in.extent(i);
    }

    this->parent = nullptr;
}

template<typename VTYPE, const int rank_t, const int* symmetry_groups>
nested_array_t<VTYPE, rank_t, symmetry_groups>::nested_array_t(const nested_array_t<VTYPE, rank_t+1, symmetry_groups>* parent_in, int index_in){

    this->data = parent_in->get_data()[index_in];

    this->c_depth = parent_in->current_depth() + 1;
    this->index = index_in;

    /** Shallow-copy extents and raggedness */
    this->extents = parent_in->get_extents();

    this->parent = parent_in;
}

template<typename VTYPE, const int rank_t, const int* symmetry_groups>
nested_array_t<VTYPE, rank_t, symmetry_groups>::~nested_array_t(){

}

template<typename VTYPE, const int rank_t, const int* symmetry_groups>
const nested_array_t<VTYPE, rank_t+1, symmetry_groups>* nested_array_t<VTYPE, rank_t, symmetry_groups>::up() const {

    if(!this->parent){
        cout << "Cannot go up, this is the top level." << endl;
        exit(-6);
    }

    return this->parent;
}

/** \brief Index into next (ragged) dimension.
 *
 * @param index_in
 * @param extent_in
 * @return
 */
template<typename VTYPE, const int rank_t, const int* symmetry_groups>
const nested_array_t<VTYPE, rank_t-1, symmetry_groups>* nested_array_t<VTYPE, rank_t, symmetry_groups>::down(int index_in, int extent_in) const {

    if(this->c_depth == rank_t){
        cout << "Cannot index any further down" << endl;
        exit(-3);
    }

    if(!this->current_raggedness()){
        cout << "This is not a ragged dimension; use '(int)' instead.";
        exit(-8);
    }else{
        nested_array_t<VTYPE, rank_t-1, symmetry_groups>* out;
        out = new nested_array_t<VTYPE, rank_t-1, symmetry_groups>(this);
        return out;
    }

    return nullptr;
}
/** \brief Index into next (non-ragged) dimension.
 *
 * @param index_in
 * @return
 */
template<typename VTYPE, const int rank_t, const int* symmetry_groups>
const nested_array_t<VTYPE, rank_t-1, symmetry_groups>* nested_array_t<VTYPE, rank_t, symmetry_groups>::down(int index_in) const {

    if(this->c_depth == rank_t){
        cout << "Cannot index any further down" << endl;
        exit(-3);
    }

    if(this->current_raggedness()){
        cout << "This is a ragged dimension; use '(int, int)' instead.";
        exit(-9);
    }else{
        nested_array_t<VTYPE, rank_t-1, symmetry_groups>* out;
        out = new nested_array_t<VTYPE, rank_t-1, symmetry_groups>(this);
        return out;
    }

    return nullptr;
}
template<typename VTYPE, const int rank_t, const int* symmetry_groups>
const nested_array_t<VTYPE, rank_t, symmetry_groups>* nested_array_t<VTYPE, rank_t, symmetry_groups>::next() const {

    if(this->index >= this->extent){
        cout << "Cannot increment, this is the last element at this level." << endl;
        exit(-7);
    }

    nested_array_t<VTYPE, rank_t, symmetry_groups>* out;
    out = new nested_array_t<VTYPE, rank_t, symmetry_groups>(this->parent, this->index + 1);
    return out;
}
template<typename VTYPE, const int rank_t, const int* symmetry_groups>
const nested_array_t<VTYPE, rank_t, symmetry_groups>* nested_array_t<VTYPE, rank_t, symmetry_groups>::first() const {

    nested_array_t<VTYPE, rank_t, symmetry_groups>* out;
    out = new nested_array_t<VTYPE, rank_t, symmetry_groups>(this->parent, 0);
    return out;
}

template<typename VTYPE, const int rank_t, const int* symmetry_groups>
const nested_array_t<VTYPE, rank_t-1, symmetry_groups> nested_array_t<VTYPE, rank_t, symmetry_groups>::operator()(int index_in, int extent_in) const {
    return *(this->down(index_in, extent_in));
}
template<typename VTYPE, const int rank_t, const int* symmetry_groups>
const nested_array_t<VTYPE, rank_t-1, symmetry_groups> nested_array_t<VTYPE, rank_t, symmetry_groups>::operator()(int index_in) const {
    return *(this->down(index_in));
}
template<typename VTYPE, const int rank_t, const int* symmetry_groups>
typename remove_pointer<typename promote<VTYPE, rank_t>::type>::type& nested_array_t<VTYPE, rank_t, symmetry_groups>::operator[](int index_in){
    return this->data[index_in];
}
template<typename VTYPE, const int rank_t, const int* symmetry_groups>
const typename remove_pointer<typename promote<VTYPE, rank_t>::type>::type& nested_array_t<VTYPE, rank_t, symmetry_groups>::operator[](int index_in) const {
    return this->data[index_in];
}
template<typename VTYPE, const int rank_t, const int* symmetry_groups>
void nested_array_t<VTYPE, rank_t, symmetry_groups>::operator=(TYPE data_in){
    this->data = data_in;
}
template<typename VTYPE, const int rank_t, const int* symmetry_groups>
nested_array_t<VTYPE, rank_t, symmetry_groups> nested_array_t<VTYPE, rank_t, symmetry_groups>::operator+(nested_array_t<VTYPE, rank_t, symmetry_groups> array_in){
    //TODO
}
template<typename VTYPE, const int rank_t, const int* symmetry_groups>
nested_array_t<VTYPE, rank_t, symmetry_groups>* nested_array_t<VTYPE, rank_t, symmetry_groups>::operator/(int num_in){
    //TODO
}


#endif
