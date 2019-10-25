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

    int* dim_order;

    size_t* extents; //< Length of dimensions; no ragged dimensions allowed

    int file_ncid;
    int var_ncid;
    nc_type var_nctype;
    int* dim_ncids;
    string* dim_names;
    int* dim_var_ncids;
    nc_type* dim_var_types;
    void** dim_var_vals;

public:
    nested_netcdf_base_t(){};
    virtual ~nested_netcdf_base_t(){};

    typedef TYPE type;
    typedef VTYPE value_type;
    static constexpr const char* file_name = FNAME;
    static constexpr const char* variable_name = VNAME;
    static constexpr const int* symmetry = symmetry_groups;
    static constexpr const int rank = rank_t;

    virtual void read() {};
    virtual void allocate() {};
    virtual void write_init(nested_netcdf_base_t*, const int) const {};
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
    void set_extent(int, int);

    constexpr int current_symmetry_group() const;
    constexpr int next_symmetry_group() const;
    constexpr int symmetry_group(int) const;

    int get_dim_order(int) const;
    int* get_dim_order() const;

    const int file_id() const;
    const int var_id() const;
    void      set_var_id(const int);
    nc_type   var_type() const;
    void      set_var_type(const nc_type);
    int*      dim_ids() const;
    int       dim_id(int) const;
    string*   get_dim_names() const;
    string    dim_name(int) const;
    int*      get_dim_var_ids() const;
    int       dim_var_id(int) const;
    nc_type*  get_dim_var_types() const;
    nc_type   dim_var_type(int) const;
    void**    get_dim_var_values() const;
    void*     dim_var_values(int) const;

    void check_dim_completeness() const;
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
    void allocate();
    template<typename NITYPE> void write_init(NITYPE* iarray_in, const int forank_in);
    void write();

    nested_netcdf_array_t<VTYPE, rank_t-1, FNAME, VNAME, symmetry_groups> operator()(int) const; //< alias for 'down', plus a dereference; non-ragged state
    DTYPE&                operator[](int);
    const DTYPE&          operator[](int) const;

    void                  operator=(TYPE); //< Shallow-copy data into this array
    void                  operator=(nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>);
    nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>  operator+(nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>); //< merge along this dimension
    nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>* operator/(int);                    //< split along this dimension

    const nested_netcdf_array_t<VTYPE, rank_t+1, FNAME, VNAME, symmetry_groups>* up() const;
    nested_netcdf_array_t<VTYPE, rank_t-1, FNAME, VNAME, symmetry_groups>* down(int) const;
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
void nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::set_extent(int depth_in, int extent_in) {
    this->extents[depth_in] = extent_in;
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
void nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::set_var_id(const int var_id_in) {
    this->var_ncid = var_id_in;
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
nc_type nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::var_type() const {
    return this->var_nctype;
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
void nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::set_var_type(const nc_type type_in) {
    this->var_nctype = type_in;
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
string* nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::get_dim_names() const {
    return this->dim_names;
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
string nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::dim_name(int depth_in) const {
    return this->dim_names[depth_in];
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
int* nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::get_dim_var_ids() const {
    return this->dim_var_ncids;
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
int nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::dim_var_id(int depth_in) const {
    return this->dim_var_ncids[depth_in];
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
nc_type* nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::get_dim_var_types() const {
    return this->dim_var_types;
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
nc_type nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::dim_var_type(int depth_in) const {
    return this->dim_var_types[depth_in];
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
void* nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::dim_var_values(int depth_in) const {
    return this->dim_var_vals[depth_in];
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
void** nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::get_dim_var_values() const {
    return this->dim_var_vals;
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
void nested_netcdf_base_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::check_dim_completeness() const {
    return;
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::nested_netcdf_array_t(int* dim_order_in){

    // OK to use runtime if/else statements here because this constructor is only called for the top level

    /** Deep copy dimension order */
    this->dim_order = new int[rank_t];
    for(int i = 0; i < rank_t; i++){
        if(!dim_order_in){
            this->dim_order[i] = i+1;
        }else{
            this->dim_order[i] = dim_order_in[i];
        }
    }

    this->dim_ncids = new int[rank_t];
    this->dim_names = new string[rank_t];
    this->extents = new size_t[rank_t];
    this->dim_var_ncids = new int[rank_t];
    this->dim_var_types = new nc_type[rank_t];
    this->dim_var_vals = new void*[rank_t];

    /** Open file and variable */
    int status = nc_create(FNAME, NC_NOCLOBBER, &(this->file_ncid));
    if (status == NC_EEXIST) {
        // The file already exists; assume read mode; read/write edge case not implemented because programming is hard

        nc_open(FNAME, NC_WRITE|NC_SHARE, &(this->file_ncid));
        nc_inq_varid(this->file_ncid, VNAME, &(this->var_ncid));

        /** Verify variable rank */
        int* num_dims = new int(0);
        nc_inq_varndims(this->file_ncid, this->var_ncid, num_dims);
        if(rank_t != *num_dims){
            cout << "Forward-declared rank does not match rank of variable in file." << endl;
            exit(-9001);
        }

        /** Query variable type */
        nc_inq_vartype(this->file_ncid, this->var_ncid, &(this->var_nctype));

        /** Query dimension IDs */
        nc_inq_vardimid(this->file_ncid, this->var_ncid, this->dim_ncids);

        /** Query dimension names */
        for(int i =0; i < rank_t; i++){
            char* tmp = new char[NC_MAX_NAME+1];
            nc_inq_dimname(this->file_ncid, this->dim_ncids[i], tmp);
            this->dim_names[i] = string(tmp);
        }

        /** Query extents */
        for(int i = 0; i < rank_t; i++){
            nc_inq_dimlen(this->file_ncid, this->dim_ncids[i], &(this->extents[i]));
        }

        /** Query dimension variable IDs */
        for(int i =0; i < rank_t; i++){
            nc_inq_varid(this->file_ncid, this->dim_names[i].c_str(), &(this->dim_var_ncids[i]));
        }

        /** Query dimension variable types */
        for(int i = 0; i < rank_t; i++){
            nc_inq_vartype(this->file_ncid, this->dim_var_ncids[i], &(this->dim_var_types[i]));
        }

        /** Query dimension values */
        for(int i = 0; i < rank_t; i++){
            size_t* size = new size_t;
            nc_inq_type(this->file_ncid, this->dim_var_types[i], NULL, size);
            this->dim_var_vals[i] = malloc((*size) * this->extents[i]);
            nc_get_var(this->file_ncid, this->dim_var_ncids[i], this->dim_var_vals[i]);
        }

    } else {
        // The file does not exist; assume write mode
        nc_create(FNAME, NC_WRITE|NC_SHARE, &(this->file_ncid));

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
    this->dim_names = new string[rank_t];
    this->dim_var_ncids = new int[rank_t];
    this->dim_var_types = new nc_type[rank_t];
    this->dim_var_vals = new void*[rank_t];
    for(int i = 0; i < rank_t; i++){
        this->extents[i] = array_in.extent(i);
        this->dim_ncids[i] = array_in.dim_id(i);
        this->indices[i] = array_in.get_indices(i);
        this->dim_order[i] = array_in.get_dim_order(i);
        this->dim_names[i] = array_in.dim_name(i);
        this->dim_var_ncids[i] = array_in.dim_var_id(i);
        this->dim_var_types[i] = array_in.dim_var_type(i);
        this->dim_var_vals[i] = array_in.dim_var_values(i);
    }

    this->data = array_in.get_data();

    this->parent = array_in.up();

}


template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::nested_netcdf_array_t(const nested_netcdf_array_t<VTYPE, rank_t+1, FNAME, VNAME, symmetry_groups>* parent_in, int index_in){

    this->file_ncid = parent_in->file_id();
    this->var_ncid = parent_in->var_id();
    this->var_nctype = parent_in->var_type();

    this->c_depth = parent_in->current_depth() + 1;

    /** Shallow-copy extents, dimension IDs, and dimension order */
    this->extents = parent_in->get_extents();
    this->dim_ncids = parent_in->dim_ids();
    this->dim_order = parent_in->get_dim_order();
    this->dim_names = parent_in->get_dim_names();
    this->dim_var_ncids = parent_in->get_dim_var_ids();
    this->dim_var_types = parent_in->get_dim_var_types();
    this->dim_var_vals = parent_in->get_dim_var_values();

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

    // *must* do this because NetCDF only accepts 1-D arrays
    if constexpr (rank_t == 1) {

        size_t starts[rank_t + this->c_depth];
        size_t counts[rank_t + this->c_depth];
        size_t data_size = 1;
        int dim_ind;

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

        this->data = new DTYPE[data_size];

        nc_get_vara(this->file_ncid, this->var_ncid, starts, counts, this->data);

    } else {

        for (size_t i = 0; i < this->current_extent(); i++) {
            this->down(i)->read();
        }

    }

}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
template<typename NITYPE> void nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::write_init(NITYPE* iarray_in, const int forank_in){

    this->set_var_type(iarray_in->var_type());

    for(int i = 0; i < rank_t - forank_in; i++){
        nc_def_dim(this->file_id(), iarray_in->dim_name(i).c_str(), iarray_in->extent(i), &(this->dim_ids()[i]));
        nc_def_var(this->file_id(), iarray_in->dim_name(i).c_str(), iarray_in->dim_var_type(i), 1, &(this->dim_ids()[i]), &(this->get_dim_var_ids()[i]));
        nc_put_var(this->file_id(), this->dim_var_id(i), iarray_in->dim_var_values(i));
    }

}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
void nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::allocate() {

    if constexpr (rank_t > 1) {
        for (size_t i = 0; i < this->current_extent(); i++) {
            this->down(i)->allocate();
        }

    } else if constexpr (rank_t == 1) {
        this->data = new DTYPE[this->current_extent()];

    } else if constexpr (rank_t == 0) {
        this->data = new TYPE;
    }
}

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
void nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::write() {

    // *must* do this because NetCDF only accepts 1-D arrays
    if constexpr (rank_t == 1) {

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

        #pragma omp critical
        nc_put_vara(this->file_ncid, this->var_ncid, starts, counts, this->data);

    } else {

        for (size_t i = 0; i < this->current_extent(); i++) {
            this->down(i)->write();
        }

    }

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

/** \brief Index into next (non-ragged) dimension.
 *
 * @param index_in
 * @return
 */

template<typename VTYPE, const int rank_t, const char FNAME[], const char VNAME[], const int* symmetry_groups>
nested_netcdf_array_t<VTYPE, rank_t-1, FNAME, VNAME, symmetry_groups>* nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::down(int index_in) const {

    if(this->c_depth == rank_t){
        cout << "Cannot index any further down" << endl;
        exit(-3);
    }

    this->indices[this->dim_order[this->c_depth]-1] = index_in;
    return new nested_netcdf_array_t<VTYPE, rank_t-1, FNAME, VNAME, symmetry_groups>(this, index_in);
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
nested_netcdf_array_t<VTYPE, rank_t-1, FNAME, VNAME, symmetry_groups> nested_netcdf_array_t<VTYPE, rank_t, FNAME, VNAME, symmetry_groups>::operator()(int index_in) const {
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
