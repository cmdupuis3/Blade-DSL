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

#ifndef NESTED_NETCDF_ARRAY_CPP
#define NESTED_NETCDF_ARRAY_CPP

#include <iostream>
#include <string>
#include <type_traits>

#include "netcdf.h"

using std::add_pointer;
using std::cout;
using std::endl;
using std::remove_pointer;
using std::string;

#include "nested_array_utilities.cpp"

using namespace nested_array_utilities;

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
class nested_netcdf_base_t {

protected:

    typedef typename promote<VTYPE, rank_t>::type TYPE;

    TYPE data;

    int c_depth;
    int* indices; // Current index

    size_t* extents; //< Length of dimensions; A value of -1 signifies a ragged dimension
    string* dim_names;

    int file_ncid;
    int var_ncid;
    int* dim_ncids;
    int* dim_order;

public:
    nested_netcdf_base_t(){};
    virtual ~nested_netcdf_base_t(){};

    typedef TYPE type;
    typedef VTYPE value_type;
    static constexpr const char* file_name = FNAME;
    static constexpr const char* variable_name = VNAME;
    static constexpr const int* symmetry = symmetry_groups;
    static constexpr const int rank = rank_t;

    virtual void read() const {};
    virtual void write() const {};
    auto get_data() const;
    constexpr int get_rank() const;
    int current_depth() const;
    int get_index() const;
    int get_indices(int) const;

    size_t current_extent() const;
    size_t next_extent() const;
    size_t extent(int) const;
    size_t* get_extents() const;

    bool current_raggedness() const;
    bool next_raggedness() const;
    bool raggedness(int) const;

    constexpr int current_symmetry_group() const;
    constexpr int next_symmetry_group() const;
    constexpr int symmetry_group(int) const;

    int get_dim_order(int) const;
    int* get_dim_order() const;

    const int file_id() const;
    const int var_id() const;
    int*      dim_ids() const;
    int       dim_id(int) const;
};


/** \brief A class representing a hyperblock of data of arbitrary dimensionality.
 *         May be ragged in any or all dimensions.
 */
template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups = nullptr>
class nested_netcdf_array_t : public nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups> {

private:
    typedef typename promote<VTYPE, rank_t>::type TYPE;
    typedef typename add_pointer<TYPE>::type UTYPE;
    typedef typename remove_pointer<TYPE>::type DTYPE;

protected:
    const nested_netcdf_array_t<VTYPE, rank_t+1, FNAME, VNAME, symmetry_groups>* parent;

public:
    nested_netcdf_array_t(int* dim_order = nullptr);
    nested_netcdf_array_t(const nested_netcdf_array_t<VTYPE, rank_t+1, FNAME, VNAME, symmetry_groups>*, int index = 0); //< Downward traversal pointer-type constructor; not for public consumption
    ~nested_netcdf_array_t();

    void read();
    void write();

    const nested_netcdf_array_t<VTYPE, rank_t-1, FNAME, VNAME, symmetry_groups> operator()(int, int) const; //< alias for 'down', plus a dereference; ragged state
    const nested_netcdf_array_t<VTYPE, rank_t-1, FNAME, VNAME, symmetry_groups> operator()(int) const;      //< alias for 'down', plus a dereference; non-ragged state
    DTYPE&                operator[](int);
    const DTYPE&          operator[](int) const;

    void                  operator=(TYPE); //< Shallow-copy data into this array
    void                  operator=(nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>);
    nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>  operator+(nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>); //< merge along this dimension
    nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>* operator/(int);                    //< split along this dimension

    const nested_netcdf_array_t<VTYPE, rank_t+1, FNAME, VNAME, symmetry_groups>* up() const;
    const nested_netcdf_array_t<VTYPE, rank_t-1, FNAME, VNAME, symmetry_groups>* down(int, int) const;
    const nested_netcdf_array_t<VTYPE, rank_t-1, FNAME, VNAME, symmetry_groups>* down(int) const;
    const nested_netcdf_array_t<VTYPE, rank_t,   FNAME, VNAME, symmetry_groups>* next() const;
    const nested_netcdf_array_t<VTYPE, rank_t,   FNAME, VNAME, symmetry_groups>* first() const;
};

//template<typename TYPE>
//class nested_array_asym_t : public nested_netcdf_array_t<TYPE, nullptr>{ public: using nested_netcdf_array_t<TYPE,nullptr>::nested_netcdf_array_t; };

//-----------------------------------------------------//
//
//                Base methods:
//
//-----------------------------------------------------//


template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
auto nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::get_data() const {
    return this->data; // unsafe!
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
constexpr int nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::get_rank() const {
    return rank_t;
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
int nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::current_depth() const {
    return this->c_depth;
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
int nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::get_index() const {
    return this->indices[this->c_depth];
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
int nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::get_indices(int dim_index_in) const {
    return this->indices[dim_index_in];
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
size_t nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::current_extent() const {
    return this->extents[this->c_depth];
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
size_t nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::next_extent() const {
    return this->extents[this->c_depth + 1];
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
size_t nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::extent(int depth_in) const {
    return this->extents[depth_in];
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
size_t* nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::get_extents() const {
    return this->extents;
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
bool nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::current_raggedness() const {
    return (this->extents[this->c_depth] == 0) ? true : false;
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
bool nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::next_raggedness() const {
    return (this->extents[this->c_depth + 1] == 0) ? true : false;
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
bool nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::raggedness(int depth_in) const {
    return (this->extents[depth_in] == 0) ? true : false;
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
constexpr int nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::current_symmetry_group() const {
    return symmetry_groups ? symmetry_groups[this->c_depth] : this->c_depth;
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
constexpr int nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::next_symmetry_group() const {
    return symmetry_groups ? symmetry_groups[this->c_depth + 1] : this->c_depth + 1;
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
constexpr int nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::symmetry_group(int depth_in) const {
    return symmetry_groups ? symmetry_groups[depth_in] : depth_in;
}
template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
int nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::get_dim_order(int dim_index) const {
    return this->dim_order[dim_index];
}
template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
int* nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::get_dim_order() const {
    return this->dim_order;
}
template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
const int nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::file_id() const {
    return this->file_ncid;
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
const int nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::var_id() const {
    return this->var_ncid;
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
int* nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::dim_ids() const {
    return this->dim_ncids;
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
int nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::dim_id(int depth_in) const {
    return this->dim_ncids[depth_in];
}




template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::nested_netcdf_array_t(int* dim_order_in){

    /** Open file and variable */
    nc_open(FNAME, NC_NOWRITE, &(this->file_ncid));
    nc_inq_varid(this->file_ncid, VNAME, &(this->var_ncid));

    /** Verify variable rank */
    int* num_dims = new int(0);
    nc_inq_var(this->file_ncid, this->var_ncid, NULL, NULL, num_dims, NULL, NULL);
    if(rank_t != *num_dims){
        cout << "Forward-declared rank does not match rank of variable in file." << endl;
        exit(-9001);
    }

    /** Query dimension IDs */
    this->dim_ncids = new int[rank_t];
    nc_inq_var(this->file_ncid, this->var_ncid, NULL, NULL, NULL, this->dim_ncids, NULL);

    /** Deep copy dimension order */
    this->dim_order = new int[rank_t];
    for(int i = 0; i < rank_t; i++){
        if(!dim_order_in){
            this->dim_order[i] = i+1;
        }else{
            this->dim_order[i] = dim_order_in[i];
        }
    }

    /** Query extents */
    this->extents = new size_t[rank_t];
    for(int i = 0; i < rank_t; i++){
        nc_inq_dimlen(this->file_ncid, this->dim_ncids[i], &(this->extents[i]));
    }

    this->c_depth = 0;
    this->indices = new int[rank_t];

    this->parent = nullptr;
}



template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
void nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::operator=(nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups> array_in){

    this->file_ncid = array_in.file_id();
    this->var_ncid = array_in.var_id();

    this->c_depth = array_in.current_depth();

    /** Deep-copy extents, dimension IDs, and indices */
    this->extents = new int[rank_t];
    this->dim_ncids = new int[rank_t];
    this->indices = new int[rank_t];
    this->dim_order = new int[rank_t];
    for(int i = 0; i < rank_t; i++){
        this->extents[i] = array_in.extent(i);
        this->dim_ncids[i] = array_in.dim_id(i);
        this->indices[i] = array_in.get_indices(i);
        this->dim_order[i] = array_in.get_dim_order(i);
    }

    this->data = array_in.get_data();

    this->parent = array_in.up();

}


template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::nested_netcdf_array_t(const nested_netcdf_array_t<VTYPE, rank_t+1, FNAME, VNAME, symmetry_groups>* parent_in, int index_in){

    this->file_ncid = parent_in->file_id();
    this->var_ncid = parent_in->var_id();

    this->c_depth = parent_in->current_depth() + 1;

    /** Shallow-copy extents, raggedness, dimension IDs, and dimension order */
    this->extents = parent_in->get_extents();
    this->dim_ncids = parent_in->dim_ids();
    this->dim_order = parent_in->get_dim_order();

    /** Deep-copy indices */
    this->indices = new int[rank_t + this->c_depth];
    for(int i = 0; i < this->c_depth; i++){
        this->indices[i] = parent_in->get_indices(i);
    }
    this->indices[this->c_depth] = index_in;

    this->parent = parent_in;
}


template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::~nested_netcdf_array_t(){
    if(!this->parent){
        nc_close(this->file_ncid);
    }
}


template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
void nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::read(){

    size_t starts[rank_t + this->c_depth];
    size_t counts[rank_t + this->c_depth];
    size_t data_size = 1;
    int dim_ind;

    for(int i = 0; i < rank_t + this->c_depth; i++){
        cout << this->dim_order[i] << endl;
    }

    for(int i = 0; i < rank_t + this->c_depth; i++){
        dim_ind = this->dim_order[i]-1;
        if(dim_ind < this->c_depth){
            starts[dim_ind] = this->indices[dim_ind];
            counts[dim_ind] = 1;
        }else{
            starts[dim_ind] = 0;
            counts[dim_ind] = this->extents[dim_ind];
            data_size *= this->extents[dim_ind];
        }
    }

    for(int i = 0; i < rank_t + this->c_depth; i++){
        cout << starts[i] << "    " << counts[i] << endl;
    }

    this->data = new DTYPE[data_size];

    nc_get_vara(this->file_ncid, this->var_ncid, starts, counts, this->data);

}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
void nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::write(){

    size_t starts[rank_t + this->c_depth];
    size_t counts[rank_t + this->c_depth];
    int dim_ind;
    for(int i = 0; i < rank_t + this->c_depth; i++){
        dim_ind = this->dim_order[i]-1;
        if(dim_ind < this->c_depth){
            starts[dim_ind] = this->indices[dim_ind];
            counts[dim_ind] = 1;
        }else{
            starts[dim_ind] = 1;
            counts[dim_ind] = this->extents[dim_ind];
        }
    }

    nc_put_vara(this->file_ncid, this->var_ncid, starts, counts, this->data);

}


template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
const nested_netcdf_array_t<VTYPE, rank_t+1, FNAME, VNAME, symmetry_groups>* nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::up() const {
/*
    if(!this->parent){
        cout << "Cannot go up, this is the top level." << endl;
        exit(-6);
    }
*/
    return this->parent;
}

/** \brief Index into next (ragged) dimension.
 *
 * @param index_in
 * @param extent_in
 * @return
 */

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
const nested_netcdf_array_t<VTYPE, rank_t-1, FNAME, VNAME, symmetry_groups>* nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::down(int index_in, int extent_in) const {

    if(this->c_depth == rank_t){
        cout << "Cannot index any further down" << endl;
        exit(-3);
    }

    if(!this->current_raggedness()){
        cout << "This is not a ragged dimension; use '(int)' instead.";
        exit(-8);
    }else{
        this->indices[this->dim_order[this->c_depth]-1] = index_in;
        this->extents[this->dim_order[this->c_depth+1]-1] = extent_in;
        nested_netcdf_array_t<VTYPE, rank_t-1, FNAME, VNAME, symmetry_groups>* out;
        out = new nested_netcdf_array_t<VTYPE, rank_t-1, FNAME, VNAME, symmetry_groups>(this, index_in);

        return out;
    }

    return nullptr;
}
/** \brief Index into next (non-ragged) dimension.
 *
 * @param index_in
 * @return
 */

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
const nested_netcdf_array_t<VTYPE, rank_t-1, FNAME, VNAME, symmetry_groups>* nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::down(int index_in) const {

    if(this->c_depth == rank_t){
        cout << "Cannot index any further down" << endl;
        exit(-3);
    }

    if(this->current_raggedness()){
        cout << "This is a ragged dimension; use '(int, int)' instead.";
        exit(-9);
    }else{
        this->indices[this->dim_order[this->c_depth]-1] = index_in;
        nested_netcdf_array_t<VTYPE, rank_t-1, FNAME, VNAME, symmetry_groups>* out;
        out = new nested_netcdf_array_t<VTYPE, rank_t-1, FNAME, VNAME, symmetry_groups>(this, index_in);
        return out;
    }

    return nullptr;
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
const nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>* nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::next() const {

    if(this->get_index() >= this->extent()){
        cout << "Cannot increment, this is the last element at this level." << endl;
        exit(-7);
    }

    nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>* out;
    out = new nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>(this->parent, this->get_index() + 1);
    return out;
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
const nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>* nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::first() const {

    nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>* out;
    out = new nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>(this->parent, 0);
    return out;
}


template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
const nested_netcdf_array_t<VTYPE, rank_t-1, FNAME, VNAME, symmetry_groups> nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::operator()(int index_in, int extent_in) const {
    return *(this->down(index_in, extent_in));
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
const nested_netcdf_array_t<VTYPE, rank_t-1, FNAME, VNAME, symmetry_groups> nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::operator()(int index_in) const {
    return *(this->down(index_in));
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
typename remove_pointer<typename promote<VTYPE, rank_t>::type>::type& nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::operator[](int index_in){
    return this->data[index_in];
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
const typename remove_pointer<typename promote<VTYPE, rank_t>::type>::type& nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::operator[](int index_in) const {
    return this->data[index_in];
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
void nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::operator=(TYPE data_in){
    this->data = data_in;
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups> nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::operator+(nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups> array_in){
    //TODO
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>* nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::operator/(int num_in){
    //TODO
}


#endif
