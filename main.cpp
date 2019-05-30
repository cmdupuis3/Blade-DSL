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

#include <functional>
#include <iostream>
#include <tuple>
#include <typeinfo>
#include <vector>

using std::cout;
using std::endl;
using std::rank;
using std::tuple;
using std::vector;

#include "nested_array.cpp"
#include "nested_netcdf_array.cpp"
#include "object_for.cpp"
#include "method_for.cpp"
#include "closure.cpp"


namespace sample_closures {

    /** Sample closure that adds ten to the input array */
    void add10(int num_in, float* array_in, float* array_out){
        for(int i = 0; i < num_in; i++){
            array_out[i] = array_in[i] + 10;
        }

        return;
    }

    /** Sample multiclosure that adds multiple arrays together elementwise */
    void add(int arity_in, int num_in, float** arrays_in, float* array_out){
        for(int i = 0; i < num_in; i++){
            array_out[i] = 0;
            for(int j = 0; j < arity_in; j++){
                array_out[i] += arrays_in[j][i];
            }
        }

        return;
    }

    /** Sample closure that adds ten to the input array (for nested arrays) */
    template<const int* ISYMMETRY = nullptr>
    struct add10_nested : closure_base_unary_t<float, 1, float, 1>{

        static constexpr const void(*function)(nested_array_t<float, 1, ISYMMETRY>, nested_array_t<float, 1>) =
            [](nested_array_t<float, 1, ISYMMETRY> iarray_in, nested_array_t<float, 1> oarray_in) -> const void {
                int num = iarray_in.current_extent();
                for(int i = 0; i < num; i++){
                    oarray_in[i] = iarray_in[i] + 10;
                }
            };

    };


    template<const int* ISYMMETRY = nullptr>
    void add10_nested_vec(vector<nested_array_t<float, 1, ISYMMETRY> > array_in, nested_array_t<float, 1> array_out){
        int num = array_in[0].current_extent();
        //float* data_out = new float[num];
        for(int i = 0; i < num; i++){
            array_out[i] = array_in[0][i] + 10;
            cout << array_in[0][i] << " + " << 10 << " = " << array_out[i] << endl;
        }

        //array_out = data_out;
        return;
    }

    /** Sample multiclosure that adds multiple arrays together elementwise (for nested arrays) */
    template<const int* ISYMMETRY = nullptr>
    void add_nested(int arity_in, nested_array_t<float, 1, ISYMMETRY>* arrays_in, nested_array_t<float, 1> array_out){
        int num = arrays_in[0].current_extent();
        for(int i = 0; i < num; i++){
            array_out[i] = 0;
            for(int j = 0; j < arity_in; j++){
                array_out[i] += arrays_in[j][i];
            }
        }

        return;
    }

    /** Sample multiclosure that adds multiple arrays together elementwise (for nested arrays) */
    template<const int* ISYMMETRY = nullptr>
    void add_nested_vec(int arity_in, vector<nested_array_t<float, 1, ISYMMETRY> > arrays_in, nested_array_t<float, 1> array_out){

        //assert(arity_in == arrays_in.size());

        int num = arrays_in[0].current_extent();
        for(int i = 0; i < num; i++){
            array_out[i] = 0;
            for(int j = 0; j < arity_in; j++){
                array_out[i] += arrays_in[j][i];
            }
        }

        return;
    }

    template<const int* ISYMMETRY = nullptr>
    struct speed_test : closure_base_unary_t<float, 1, float, 1>{

        static constexpr const void(*function)(nested_array_t<float, 1, ISYMMETRY>, nested_array_t<float, 1>) =
            [](nested_array_t<float, 1, ISYMMETRY> iarray_in, nested_array_t<float, 1> oarray_in) -> const void {

                int ct = 0;
                for(int i = 0; i < 500; i++){
                    ct += i;
                }
                while (ct > 0){
                    ct--;
                }

                for(int i = 0; i < iarray_in.current_extent(); i++){
                    oarray_in[i] = iarray_in[i];
                }
            };

    };

}


bool test1(){

    using namespace sample_closures;

    srand(time(NULL)+1);

    int rank = 3;
    const int len1 = 3;
    const int len2 = 3;
    const int len3 = 20;

    int* extents = new int[rank];
    extents[0] = len1;
    extents[1] = len2;
    extents[2] = len3;

    float*** in = new float**[len1];
    for(int i = 0; i < len1; i++){
        in[i] = new float*[len2];
        for(int j = 0; j < len2; j++){
            in[i][j] = new float[len3];
            for(int k = 0; k < len3; k++){
                in[i][j][k] = rand() % 10;
            }
        }
    }

    float*** out = new float**[len1];
    for(int i = 0; i < len1; i++){
        out[i] = new float*[len2];
        for(int j = 0; j < len2; j++){
            out[i][j] = new float[len3];
        }
    }

    nested_array_t<float, 3>  nin(extents,  in);
    nested_array_t<float, 3> nout(extents, out);

    auto a = object_for_t<decltype(nin), decltype(nout), closure_base_unary_t<float, 1, float, 1> >(add10_nested<>::function);

    //auto b = a();
    a(nin, nout);

    for(int i = 0; i < len1; i++){
        for(int j = 0; j < len2; j++){
            cout << nin[i][j][0] << " + " << 10 << " = " << nout[i][j][0] << endl;
        }
        cout << endl;
    }

    return true;
}

int speed_test1(){

    using namespace sample_closures;

    srand(time(NULL)+1);

    int rank = 3;
    const int len1 = 1000;
    const int len2 = 1000;
    const int len3 = 20;

    int* extents = new int[rank];
    extents[0] = len1;
    extents[1] = len2;
    extents[2] = len3;

    float*** in = new float**[len1];
    for(int i = 0; i < len1; i++){
        in[i] = new float*[len2];
        for(int j = 0; j < len2; j++){
            in[i][j] = new float[len3];
        }
    }

    for(int i = 0; i < len1; i++){
        for(int j = 0; j < len2; j++){
            for(int k = 0; k < len3; k++){
                in[i][j][k] = rand() % 10;
            }
        }
    }

    float*** out = new float**[len1];
    for(int i = 0; i < len1; i++){
        out[i] = new float*[len2];
        for(int j = 0; j < len2; j++){
            out[i][j] = new float[len3];
        }
    }

    nested_array_t<float, 3>  nin(extents,  in);
    nested_array_t<float, 3> nout(extents, out);

    auto a = object_for_t<decltype(nin), decltype(nout), closure_base_unary_t<float, 1, float, 1> >(speed_test<>::function);

    time_t kstart = time(nullptr);
    a(nin, nout);
    time_t kend = time(nullptr);

    double ktime = difftime(kend,kstart);
    cout << "speed test 1 (no OpenMP):" << ktime << "s; " << endl;

    return ktime;
}


int speed_test1_omp(){

    using namespace sample_closures;

    srand(time(NULL)+1);

    int rank = 3;
    const int len1 = 1000;
    const int len2 = 1000;
    const int len3 = 20;

    int* extents = new int[rank];
    extents[0] = len1;
    extents[1] = len2;
    extents[2] = len3;

    float*** in = new float**[len1];
    for(int i = 0; i < len1; i++){
        in[i] = new float*[len2];
        for(int j = 0; j < len2; j++){
            in[i][j] = new float[len3];
        }
    }

    for(int i = 0; i < len1; i++){
        for(int j = 0; j < len2; j++){
            for(int k = 0; k < len3; k++){
                in[i][j][k] = rand() % 10;
            }
        }
    }

    float*** out = new float**[len1];
    for(int i = 0; i < len1; i++){
        out[i] = new float*[len2];
        for(int j = 0; j < len2; j++){
            out[i][j] = new float[len3];
        }
    }

    nested_array_t<float, 3>  nin(extents,  in);
    nested_array_t<float, 3> nout(extents, out);

    static constexpr const int omp_levels = 1;
    auto a = object_for_t<decltype(nin), decltype(nout), closure_base_unary_t<float, 1, float, 1>, omp_levels>(speed_test<>::function);

    time_t kstart = time(nullptr);
    a(nin, nout);
    time_t kend = time(nullptr);

    double ktime = difftime(kend,kstart);
    cout << "speed test 1 (w/ OpenMP):" << ktime << "s; " << endl;

    return ktime;
}

bool test2(){

    using namespace sample_closures;

    srand(time(NULL)+2);

    int rank = 3;
    const int len1 = 3;
    const int len2 = 3;
    const int len3 = 20;

    int* extents = new int[rank];
    extents[0] = len1;
    extents[1] = len2;
    extents[2] = len3;

    float*** in = new float**[len1];
    for(int i = 0; i < len1; i++){
        in[i] = new float*[len2];
        for(int j = 0; j < len2; j++){
            in[i][j] = new float[len3];
            for(int k = 0; k < len3; k++){
                in[i][j][k] = rand() % 10;
            }
        }
    }

    float*** out = new float**[len1];
    for(int i = 0; i < len1; i++){
        out[i] = new float*[len2];
        for(int j = 0; j < len2; j++){
            out[i][j] = new float[len3];
        }
    }

    nested_array_t<float, 3>  nin(extents,  in);
    nested_array_t<float, 3> nout(extents, out);

    //closure_base_unary_t<nested_array_t<float, 1>, nested_array_t<float, 1>, add10_nested> cl;

    auto b = method_for_t<decltype(nin), decltype(nout), closure_base_unary_t<float, 1, float, 1> >(nin);

    //auto c = b();
    b(add10_nested<>::function, nout);

    for(int i = 0; i < len1; i++){
        for(int j = 0; j < len2; j++){
            cout << nin[i][j][0] << " + " << 10 << " = " << nout[i][j][0] << endl;
        }
        cout << endl;
    }

    return true;
}

int speed_test2(){

    using namespace sample_closures;

    srand(time(NULL)+1);

    int rank = 3;
    const int len1 = 1000;
    const int len2 = 1000;
    const int len3 = 20;

    int* extents = new int[rank];
    extents[0] = len1;
    extents[1] = len2;
    extents[2] = len3;

    float*** in = new float**[len1];
    for(int i = 0; i < len1; i++){
        in[i] = new float*[len2];
        for(int j = 0; j < len2; j++){
            in[i][j] = new float[len3];
        }
    }

    for(int i = 0; i < len1; i++){
        for(int j = 0; j < len2; j++){
            for(int k = 0; k < len3; k++){
                in[i][j][k] = rand() % 10;
            }
        }
    }

    float*** out = new float**[len1];
    for(int i = 0; i < len1; i++){
        out[i] = new float*[len2];
        for(int j = 0; j < len2; j++){
            out[i][j] = new float[len3];
        }
    }

    nested_array_t<float, 3>  nin(extents,  in);
    nested_array_t<float, 3> nout(extents, out);

    auto a = method_for_t<decltype(nin), decltype(nout), closure_base_unary_t<float, 1, float, 1> >(nin);

    time_t kstart = time(nullptr);
    a(speed_test<>::function, nout);
    time_t kend = time(nullptr);

    double ktime = difftime(kend,kstart);
    cout << "speed test 2 (no OpenMP):" << ktime << "s; " << endl;

    return ktime;
}


int speed_test2_omp(){

    using namespace sample_closures;

    srand(time(NULL)+1);

    int rank = 3;
    const int len1 = 1000;
    const int len2 = 1000;
    const int len3 = 20;

    int* extents = new int[rank];
    extents[0] = len1;
    extents[1] = len2;
    extents[2] = len3;

    float*** in = new float**[len1];
    for(int i = 0; i < len1; i++){
        in[i] = new float*[len2];
        for(int j = 0; j < len2; j++){
            in[i][j] = new float[len3];
        }
    }

    for(int i = 0; i < len1; i++){
        for(int j = 0; j < len2; j++){
            for(int k = 0; k < len3; k++){
                in[i][j][k] = rand() % 10;
            }
        }
    }

    float*** out = new float**[len1];
    for(int i = 0; i < len1; i++){
        out[i] = new float*[len2];
        for(int j = 0; j < len2; j++){
            out[i][j] = new float[len3];
        }
    }

    nested_array_t<float, 3>  nin(extents,  in);
    nested_array_t<float, 3> nout(extents, out);

    static constexpr const int omp_levels = 1;
    auto a = method_for_t<decltype(nin), decltype(nout), closure_base_unary_t<float, 1, float, 1>, omp_levels>(nin);

    time_t kstart = time(nullptr);
    a(speed_test<>::function, nout);
    time_t kend = time(nullptr);

    double ktime = difftime(kend,kstart);
    cout << "speed test 2 (w/ OpenMP):" << ktime << "s; " << endl;

    return ktime;
}


bool test3(){

    using namespace sample_closures;

    srand(time(NULL)+3);

    int rank = 3;
    const int len1 = 5;
    const int len2 = 20;

    int* extents = new int[rank];
    extents[0] = len1;
    extents[1] = len1;
    extents[2] = len2;

    float*** in = new float**[len1];
    for(int i = 0; i < len1; i++){
        in[i] = new float*[len1];
        for(int j = 0; j < len1; j++){
            in[i][j] = new float[len2];
        }
    }

    for(int i = 0; i < len1; i++){
        for(int j = i; j < len1; j++){
            for(int k = 0; k < len2; k++){
                in[i][j][k] = rand() % 10;
            }
        }
        // We don't need this because the symmetry vector tells the method_for loop to never use the lower triangle anyway.
        //for(int j = 0; j < i; j++){
        //    for(int k = 0; k < len2; k++){
        //        in[i][j][k] = in[j][i][k];
        //    }
        //}
    }

    float*** out = new float**[len1];
    for(int i = 0; i < len1; i++){
        out[i] = new float*[len1];
        for(int j = i; j < len1; j++){
            out[i][j] = new float[len2];
        }
        for(int j = 0; j < i; j++){
            out[i][j] = out[j][i];
        }
    }

    static constexpr int symmetry[3] = {1,1,2};
    nested_array_t<float, 3, symmetry>  nin(extents,  in);
    nested_array_t<float, 3>           nout(extents, out);

    auto a = object_for_t<decltype(nin), decltype(nout), closure_base_unary_t<float, 1, float, 1> >(add10_nested<symmetry>::function);

    a(nin, nout);

    for(int i = 0; i < len1; i++){
        for(int j = 0; j < len1; j++){
            cout << out[i][j][0] << "\t";
        }
        cout << endl;
    }
    cout << endl;

    return true;
}


int speed_test3(){

    using namespace sample_closures;

    srand(time(NULL)+3);

    int rank = 3;
    const int len1 = 1000;
    const int len2 = 20;

    int* extents = new int[rank];
    extents[0] = len1;
    extents[1] = len1;
    extents[2] = len2;

    float*** in = new float**[len1];
    for(int i = 0; i < len1; i++){
        in[i] = new float*[len1];
        for(int j = 0; j < len1; j++){
            in[i][j] = new float[len2];
        }
    }

    for(int i = 0; i < len1; i++){
        for(int j = i; j < len1; j++){
            for(int k = 0; k < len2; k++){
                in[i][j][k] = rand() % 10;
            }
        }
        // We don't need this because the symmetry vector tells the method_for loop to never use the lower triangle anyway.
        //for(int j = 0; j < i; j++){
        //    for(int k = 0; k < len2; k++){
        //        in[i][j][k] = in[j][i][k];
        //    }
        //}
    }

    float*** out = new float**[len1];
    for(int i = 0; i < len1; i++){
        out[i] = new float*[len1];
        for(int j = i; j < len1; j++){
            out[i][j] = new float[len2];
        }
        for(int j = 0; j < i; j++){
            out[i][j] = out[j][i];
        }
    }

    static constexpr int symmetry[3] = {1,1,2};
    nested_array_t<float, 3, symmetry>  nin(extents,  in);
    nested_array_t<float, 3>           nout(extents, out);

    auto a = object_for_t<decltype(nin), decltype(nout), closure_base_unary_t<float, 1, float, 1> >(speed_test<symmetry>::function);

    time_t kstart = time(nullptr);
    a(nin, nout);
    time_t kend = time(nullptr);

    double ktime = difftime(kend,kstart);
    cout << "speed test 3 (no OpenMP):" << ktime << "s; " << endl;

    return ktime;
}


int speed_test3_omp(){

    using namespace sample_closures;

    srand(time(NULL)+3);

    int rank = 3;
    const int len1 = 1000;
    const int len2 = 20;

    int* extents = new int[rank];
    extents[0] = len1;
    extents[1] = len1;
    extents[2] = len2;

    float*** in = new float**[len1];
    for(int i = 0; i < len1; i++){
        in[i] = new float*[len1];
        for(int j = 0; j < len1; j++){
            in[i][j] = new float[len2];
        }
    }

    for(int i = 0; i < len1; i++){
        for(int j = i; j < len1; j++){
            for(int k = 0; k < len2; k++){
                in[i][j][k] = rand() % 10;
            }
        }
        // We don't need this because the symmetry vector tells the method_for loop to never use the lower triangle anyway.
        //for(int j = 0; j < i; j++){
        //    for(int k = 0; k < len2; k++){
        //        in[i][j][k] = in[j][i][k];
        //    }
        //}
    }

    float*** out = new float**[len1];
    for(int i = 0; i < len1; i++){
        out[i] = new float*[len1];
        for(int j = i; j < len1; j++){
            out[i][j] = new float[len2];
        }
        for(int j = 0; j < i; j++){
            out[i][j] = out[j][i];
        }
    }

    static constexpr int symmetry[3] = {1,1,2};
    nested_array_t<float, 3, symmetry>  nin(extents,  in);
    nested_array_t<float, 3>           nout(extents, out);

    static constexpr const int omp_levels = 1;
    auto a = object_for_t<decltype(nin), decltype(nout), closure_base_unary_t<float, 1, float, 1>, omp_levels>(speed_test<symmetry>::function);

    time_t kstart = time(nullptr);
    a(nin, nout);
    time_t kend = time(nullptr);

    double ktime = difftime(kend,kstart);
    cout << "speed test 3 (w/ OpenMP):" << ktime << "s; " << endl;

    return ktime;
}

bool test4(){

    using namespace sample_closures;

    srand(time(NULL)+4);

    int rank = 3;
    const int len1 = 5;
    const int len2 = 20;

    int* extents = new int[rank];
    extents[0] = len1;
    extents[1] = len1;
    extents[2] = len2;

    float*** in = new float**[len1];
    for(int i = 0; i < len1; i++){
        in[i] = new float*[len1];
        for(int j = 0; j < len1; j++){
            in[i][j] = new float[len2];
        }
    }

    for(int i = 0; i < len1; i++){
        for(int j = i; j < len1; j++){
            for(int k = 0; k < len2; k++){
                in[i][j][k] = rand() % 10;
            }
        }
        /* We don't need this because the symmetry vector tells the method_for loop to never use the lower triangle anyway. */
        //for(int j = 0; j < i; j++){
        //    for(int k = 0; k < len2; k++){
        //        in[i][j][k] = in[j][i][k];
        //    }
        //}
    }

    float*** out = new float**[len1];
    for(int i = 0; i < len1; i++){
        out[i] = new float*[len1];
        for(int j = i; j < len1; j++){
            out[i][j] = new float[len2];
        }
        for(int j = 0; j < i; j++){
            out[i][j] = out[j][i];
        }
    }

    static constexpr const int symmetry[3] = {1,1,2};
    nested_array_t<float, 3, symmetry>  nin(extents,  in);
    nested_array_t<float, 3>           nout(extents, out);

    auto b = method_for_t<decltype(nin), decltype(nout), closure_base_unary_t<float, 1, float, 1> >(nin);

    b(add10_nested<symmetry>::function, nout);

    for(int i = 0; i < len1; i++){
        for(int j = 0; j < len1; j++){
            cout << out[i][j][0] << "\t";
        }
        cout << endl;
    }
    cout << endl;


    return true;
}

int speed_test4(){

    using namespace sample_closures;

    srand(time(NULL)+4);

    int rank = 3;
    const int len1 = 1000;
    const int len2 = 20;

    int* extents = new int[rank];
    extents[0] = len1;
    extents[1] = len1;
    extents[2] = len2;

    float*** in = new float**[len1];
    for(int i = 0; i < len1; i++){
        in[i] = new float*[len1];
        for(int j = 0; j < len1; j++){
            in[i][j] = new float[len2];
        }
    }

    for(int i = 0; i < len1; i++){
        for(int j = i; j < len1; j++){
            for(int k = 0; k < len2; k++){
                in[i][j][k] = rand() % 10;
            }
        }
        /* We don't need this because the symmetry vector tells the method_for loop to never use the lower triangle anyway. */
        //for(int j = 0; j < i; j++){
        //    for(int k = 0; k < len2; k++){
        //        in[i][j][k] = in[j][i][k];
        //    }
        //}
    }

    float*** out = new float**[len1];
    for(int i = 0; i < len1; i++){
        out[i] = new float*[len1];
        for(int j = i; j < len1; j++){
            out[i][j] = new float[len2];
        }
        for(int j = 0; j < i; j++){
            out[i][j] = out[j][i];
        }
    }

    static constexpr const int symmetry[3] = {1,1,2};
    nested_array_t<float, 3, symmetry>  nin(extents,  in);
    nested_array_t<float, 3>           nout(extents, out);

    auto b = method_for_t<decltype(nin), decltype(nout), closure_base_unary_t<float, 1, float, 1> >(nin);


    time_t kstart = time(nullptr);
    b(speed_test<symmetry>::function, nout);
    time_t kend = time(nullptr);


    double ktime = difftime(kend,kstart);
    cout << "speed test 4 (no OpenMP):" << ktime << "s; " << endl;

    return ktime;
}

int speed_test4_omp(){

    using namespace sample_closures;

    srand(time(NULL)+4);

    int rank = 3;
    const int len1 = 1000;
    const int len2 = 20;

    int* extents = new int[rank];
    extents[0] = len1;
    extents[1] = len1;
    extents[2] = len2;

    float*** in = new float**[len1];
    for(int i = 0; i < len1; i++){
        in[i] = new float*[len1];
        for(int j = 0; j < len1; j++){
            in[i][j] = new float[len2];
        }
    }

    for(int i = 0; i < len1; i++){
        for(int j = i; j < len1; j++){
            for(int k = 0; k < len2; k++){
                in[i][j][k] = rand() % 10;
            }
        }
        /* We don't need this because the symmetry vector tells the method_for loop to never use the lower triangle anyway. */
        //for(int j = 0; j < i; j++){
        //    for(int k = 0; k < len2; k++){
        //        in[i][j][k] = in[j][i][k];
        //    }
        //}
    }

    float*** out = new float**[len1];
    for(int i = 0; i < len1; i++){
        out[i] = new float*[len1];
        for(int j = i; j < len1; j++){
            out[i][j] = new float[len2];
        }
        for(int j = 0; j < i; j++){
            out[i][j] = out[j][i];
        }
    }

    static constexpr int symmetry[3] = {1,1,2};
    nested_array_t<float, 3, symmetry>  nin(extents,  in);
    nested_array_t<float, 3>           nout(extents, out);

    static constexpr const int omp_levels = 1;
    auto b = method_for_t<decltype(nin), decltype(nout), closure_base_unary_t<float, 1, float, 1>, omp_levels>(nin);


    time_t kstart = time(nullptr);
    b(speed_test<symmetry>::function, nout);
    time_t kend = time(nullptr);

    double ktime = difftime(kend,kstart);
    cout << "speed test 4 (w/ OpenMP):" << ktime << "s; " << endl;

    return ktime;
}

/*
bool test5(){

    using namespace sample_closures;

    srand(time(NULL)+5);

    int rank = 5;
    const int len1 = 5;
    const int len2 = 20;

    int* extents = new int[rank];
    extents[0] = len1;
    extents[1] = len1;
    extents[2] = len2;

    float*** in1 = new float**[len1];
    float*** in2 = new float**[len1];
    for(int i = 0; i < len1; i++){
        in1[i] = new float*[len1];
        in2[i] = new float*[len1];
        for(int j = 0; j < len1; j++){
            in1[i][j] = new float[len2];
            in2[i][j] = new float[len2];
        }
    }

    for(int i = 0; i < len1; i++){
        for(int j = 0; j < len1; j++){
            for(int k = 0; k < len2; k++){
                in1[i][j][k] = rand() % 10;
                in2[i][j][k] = rand() % 10;
            }
        }
    }

    float*** out = new float**[len1];
    for(int i = 0; i < len1; i++){
        out[i] = new float*[len1];
        for(int j = 0; j < len1; j++){
            out[i][j] = new float[len2];
        }
    }

    nested_array_t<float, 3> nin1(extents, in1);
    nested_array_t<float, 3> nin2(extents, in2);
    nested_array_t<float, 3> nout(extents, out);

    auto nin = std::make_tuple(nin1, nin2);
    auto b = method_for_t<decltype(nin), decltype(nout), float*, float*>(nin, nout);

    b(add_nested_vec);

    for(int i = 0; i < len1; i++){
        for(int j = 0; j < len1; j++){
            cout << out[i][j][0] << "\t";
        }
        cout << endl;
    }

    return true;
}
*/
/*
bool test6(){

    using namespace sample_closures;

    srand(time(NULL)+6);

    int rank = 3;
    const int len1 = 5;
    const int len2 = 20;

    int* extents = new int[rank];
    extents[0] = len1;
    extents[1] = len1;
    extents[2] = len2;

    float*** in1 = new float**[len1];
    float*** in2 = new float**[len1];
    for(int i = 0; i < len1; i++){
        in1[i] = new float*[len1];
        in2[i] = new float*[len1];
        for(int j = 0; j < len1; j++){
            in1[i][j] = new float[len2];
            in2[i][j] = new float[len2];
        }
    }

    for(int i = 0; i < len1; i++){
        for(int j = 0; j < len1; j++){
            for(int k = 0; k < len2; k++){
                in1[i][j][k] = rand() % 10;
                in2[i][j][k] = rand() % 10;
            }
        }
    }

    float*** out = new float**[len1];
    for(int i = 0; i < len1; i++){
        out[i] = new float*[len1];
        for(int j = 0; j < len1; j++){
            out[i][j] = new float[len2];
        }
    }

    vector<nested_array_t<float, 1> > nin;
    nin.push_back(nested_array_t<float, 1>(&extents[2], in1[0][0]));
    nin.push_back(nested_array_t<float, 1>(&extents[2], in2[0][0]));
    nested_array_t<float, 1> nout(&extents[2], out[0][0]);


    static constexpr const int arity = 2;
    static constexpr const int commutativity[arity] = {1,1};
    closure_base_t<arity, vector<nested_array_t<float, 1> >, nested_array_t<float, 1>, add_nested_vec, commutativity> cl;



    cl(nin, nout);

    for(int k = 0; k < len2; k++){
        cout << nin.at(0)[k] << " + " << nin.at(1)[k] << " = " << nout[k] << endl;
    }

    return true;
}
*/
/*
bool test7(){

    using namespace sample_closures;

    srand(time(NULL)+7);

    int rank = 3;
    const int len1 = 5;
    const int len2 = 20;

    int* extents = new int[rank];
    extents[0] = len1;
    extents[1] = len1;
    extents[2] = len2;

    float*** in1 = new float**[len1];
    float*** in2 = new float**[len1];
    for(int i = 0; i < len1; i++){
        in1[i] = new float*[len1];
        in2[i] = new float*[len1];
        for(int j = 0; j < len1; j++){
            in1[i][j] = new float[len2];
            in2[i][j] = new float[len2];
        }
    }

    for(int i = 0; i < len1; i++){
        for(int j = 0; j < len1; j++){
            for(int k = 0; k < len2; k++){
                in1[i][j][k] = rand() % 10;
                in2[i][j][k] = rand() % 10;
            }
        }
    }

    float*** out = new float**[len1];
    for(int i = 0; i < len1; i++){
        out[i] = new float*[len1];
        for(int j = 0; j < len1; j++){
            out[i][j] = new float[len2];
        }
    }

    nested_array_t<float, 3> nin1(extents, in1);
    nested_array_t<float, 3> nin2(extents, in2);
    nested_array_t<float, 3> nout(extents, out);

    auto nin = std::make_tuple(nin1, nin2);

    static constexpr const int arity = 2;
    static constexpr const int commutativity[arity] = {1,1};
    closure_base_t<arity, vector<nested_array_t<float, 1> >, nested_array_t<float, 1>, add_nested_vec, commutativity> cl;

    auto b = method_for_chained_t<decltype(nin), decltype(nout), decltype(cl)>(nin, nout);

    b([](vector<nested_array_t<float, 1> > iarrays_in, nested_array_t<float, 1> oarray_in){
        int arity = iarrays_in.size();
        for(int i = 0; i < arity; i++){
            for(int j = 0; j < iarrays_in.at(i).current_extent(); j++){
                oarray_in[i] += iarrays_in.at(i)[j] + 10;
            }
        }
    });

    return true;
}
*/
bool test8(){

    using namespace nested_array_utilities;

    static constexpr const int extents[20] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,3,4};
    typedef promote<float, 20>::type arrtype;
    auto a = allocate<arrtype, extents>();

    fill_random<arrtype, extents>(a, 10);


    for (int i = 0; i < extents[17]; i++){
        for (int j = 0; j < extents[18]; j++) {
            for (int k = 0; k < extents[19]; k++) {
                cout << a[0][0][0][0][0][0][0][0][0][0][0][0][0][0][0][0][0][i][j][k] << " ";
            }
            cout << endl;
        }
        cout << endl;
    }
    return true;

}

template<typename TUITYPE, const int element = 0>
constexpr auto gather_symmetry(TUITYPE var){
    if constexpr (element == std::tuple_size<TUITYPE>::value - 1) {
        if constexpr (std::tuple_element<element, TUITYPE>::type::symmetry) {
            return std::make_tuple(std::tuple_element<element, TUITYPE>::type::symmetry);
        } else {
            return std::make_tuple(-1);
        }
    } else {
        if constexpr (std::tuple_element<element, TUITYPE>::type::symmetry) {
            return std::tuple_cat(std::make_tuple(std::tuple_element<element, TUITYPE>::type::symmetry), gather_symmetry<TUITYPE, element + 1>(var));
        } else {
            return std::tuple_cat(std::make_tuple(-1), gather_symmetry<TUITYPE, element + 1>(var));
        }
    }
}

int main(){

    test1();
    test2();
    test3();
    test4();
    //test5();
    //test6();
    //test8();

//    speed_test1();
//    speed_test1_omp();
//    speed_test2();
//    speed_test2_omp();
//    speed_test3();
//    speed_test3_omp();
//    speed_test4();
//    speed_test4_omp();
    
/*
    static constexpr const char fname[] = "/home/Christopher.Dupuis/EDGI_PCA/vars_a.nc";
    static constexpr const char vname[] = "SST_a";

    int order[] = {2,1,3};
    nested_netcdf_array_t<float, 3, fname, vname> block(order);

    auto a = block(40);
    auto b = a(80);
    b.read();

    for(int i = 0; i < 5; i++){
        cout << b[i] << endl;
    }
*/
    
    return 0;
}
