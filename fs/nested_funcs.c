
#include "netcdf.h"

extern int get_num_dims(char* file_name, char* variable_name) {

    int file_ncid;
    int var_ncid;
    int num_dims = -1;

    nc_open(file_name, NC_NOWRITE|NC_SHARE, &(file_ncid));
    nc_inq_varid(file_ncid, variable_name, &(var_ncid));
    nc_inq_varndims(file_ncid, var_ncid, &num_dims);

    return num_dims;

}

extern int get_var_type(char* file_name, char* variable_name) {

    int file_ncid;
    int var_ncid;
    nc_type type = -1;

    nc_open(file_name, NC_NOWRITE|NC_SHARE, &(file_ncid));
    nc_inq_varid(file_ncid, variable_name, &(var_ncid));
    nc_inq_vartype(file_ncid, var_ncid, &type);

    return (int)type;

}
