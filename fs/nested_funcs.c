
#include <stdlib.h>
#include "netcdf.h"

extern int get_num_dims(char* file_name, char* variable_name) {

    int file_ncid;
    int var_ncid;
    int num_dims = -1;

    nc_open(file_name, NC_NOWRITE|NC_SHARE, &(file_ncid));
    nc_inq_varid(file_ncid, variable_name, &(var_ncid));
    nc_inq_varndims(file_ncid, var_ncid, &num_dims);

    nc_close(file_ncid);

    return num_dims;

}

extern int get_var_type(char* file_name, char* variable_name) {

    int file_ncid;
    int var_ncid;
    nc_type type = -1;

    nc_open(file_name, NC_NOWRITE|NC_SHARE, &(file_ncid));
    nc_inq_varid(file_ncid, variable_name, &(var_ncid));
    nc_inq_vartype(file_ncid, var_ncid, &type);

    nc_close(file_ncid);

    return (int)type;

}

/* This elides the difference between a dimension and a dimension variable */
extern void get_var_dim_types(char* file_name, char* variable_name, int dim_types[]){

    int file_ncid;
    int var_ncid;
    int num_dims = -1;

    nc_open(file_name, NC_NOWRITE|NC_SHARE, &file_ncid);
    nc_inq_varid(file_ncid, variable_name, &var_ncid);
    nc_inq_varndims(file_ncid, var_ncid, &num_dims);

    int var_dimids[num_dims];

    nc_inq_vardimid(file_ncid, var_ncid, var_dimids);
    for(int i = 0; i < num_dims; i++){
        char* dimvar_name = (char*)malloc(sizeof(char) * NC_MAX_NAME);
        int dimvar_ncid = -1;
        nc_inq_dimname(file_ncid, var_dimids[i], dimvar_name);
        nc_inq_varid(file_ncid, dimvar_name, &dimvar_ncid);
        nc_inq_vartype(file_ncid, dimvar_ncid, &(dim_types[i]));

        free(dimvar_name);
    }

    nc_close(file_ncid);

    return;
}

