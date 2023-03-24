#include <string>
using std::string;
#include <omp.h>
#include <cmath>
#include <iostream>
using std::cout;
using std::endl;
#include <chrono>
#define TIME std::chrono::high_resolution_clock::now()
#define TIME_DIFF std::chrono::duration_cast<std::chrono::nanoseconds>(end-start).count()
#include "nested_array_utilities.cpp"
using namespace nested_array_utilities;
#define OMP_NUM_THREADS 10
static constexpr const size_t res_symm[7]={1,1,1,1,5,6,7};




static constexpr const size_t a1_symm[7] = {1,  1,  1,  1,  5,  6,  7};


template<typename ITYPE1, const size_t IRANK1, const size_t* ISYM1, typename OTYPE, const size_t ORANK, const size_t* OSYM> void add10(
	typename promote<ITYPE1, IRANK1>::type in1, 
	typename promote<OTYPE, ORANK>::type out, 
	const size_t in1_extents[IRANK1],
	const size_t out_extents[ORANK]){
	// Nothing to see here.
}

template<> void add10<float, 7, a1_symm, float, 7, res_symm>(
	typename promote<float, 7>::type in1, 
	typename promote<float, 7>::type out, 
	const size_t in1_extents[7], 
	const size_t out_extents[7]) {
	size_t __i0 = 0;
	#pragma omp parallel for private(__i0)
	for(__i0 = 0; __i0 < in1_extents[0] - 0; __i0++) {
		promote<float, 6>::type in1__i0 = in1[__i0];
		size_t __i1 = 0;
		#pragma omp parallel for private(__i1)
		for(__i1 = 0; __i1 < in1_extents[1] - __i0; __i1++) {
			promote<float, 5>::type in1__i1 = in1__i0[__i1];
			size_t __i2 = 0;
			
			for(__i2 = 0; __i2 < in1_extents[2] - __i1; __i2++) {
				promote<float, 4>::type in1__i2 = in1__i1[__i2];
				size_t __i3 = 0;
				
				for(__i3 = 0; __i3 < in1_extents[3] - __i2; __i3++) {
					promote<float, 3>::type in1__i3 = in1__i2[__i3];
					size_t __i4 = 0;
					
					for(__i4 = 0; __i4 < in1_extents[4] - 0; __i4++) {
						promote<float, 2>::type in1__i4 = in1__i3[__i4];
						size_t __i5 = 0;
						
						for(__i5 = 0; __i5 < in1_extents[5] - 0; __i5++) {
							promote<float, 1>::type in1__i5 = in1__i4[__i5];
							size_t __i6 = 0;
							
							for(__i6 = 0; __i6 < in1_extents[6] - 0; __i6++) {
								promote<float, 0>::type in1__i6 = in1__i5[__i6];
								out[__i0][__i1][__i2][__i3][__i4][__i5][__i6]=in1__i6+10;
								

							}
						}
					}
				}
			}
		}
	}
	}
static constexpr const size_t extents7[7]={10,10,10,10,10,10,5000};
int main(){
auto start=TIME;




promote<float, 7>::type a1;
a1 = allocate<typename promote<float, 7>::type,a1_symm>(extents7);
auto end=TIME;
double diff=1e-9*TIME_DIFF;
cout<<"Input Allocation took "<<diff<<"s"<<endl<<endl;
typedef promote<float,7>::type float7;
start=TIME;
fill_random<float7,a1_symm>(a1,extents7,7);
end=TIME;
diff=1e-9*TIME_DIFF;
cout<<"Random fill took "<<diff<<"s"<<endl<<endl;
start=TIME;
size_t* extents7_res=new size_t[7];
extents7_res[0]=extents7[0];
extents7_res[1]=extents7[1];
extents7_res[2]=extents7[2];
extents7_res[3]=extents7[3];
extents7_res[4]=extents7[4];
extents7_res[5]=extents7[5];
extents7_res[6]=extents7[6];
promote<float,7>::type res;
res=allocate<typename promote<float,7>::type,res_symm>(extents7_res);
end=TIME;
diff=1e-9*TIME_DIFF;
cout<<"Output Allocation took "<<diff<<"s"<<endl<<endl;
start=TIME;
add10<float,7,a1_symm,float,7,res_symm>(a1,res,extents7,extents7_res);
end=TIME;
diff=1e-9*TIME_DIFF;
cout<<"Calculation took "<<diff<<"s"<<endl<<endl;
return 0;
}


