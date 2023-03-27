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
#define OMP_NUM_THREADS 8
static constexpr const size_t res_symm[8]={1,1,1,1,1,1,1,2};


template<typename ITYPE1, const size_t IRANK1, const size_t* ISYM1, typename ITYPE2, const size_t IRANK2, const size_t* ISYM2, typename ITYPE3, const size_t IRANK3, const size_t* ISYM3, typename ITYPE4, const size_t IRANK4, const size_t* ISYM4, typename ITYPE5, const size_t IRANK5, const size_t* ISYM5, typename ITYPE6, const size_t IRANK6, const size_t* ISYM6, typename ITYPE7, const size_t IRANK7, const size_t* ISYM7, typename ITYPE8, const size_t IRANK8, const size_t* ISYM8, typename OTYPE, const size_t ORANK, const size_t* OSYM> void add10(
	typename promote<ITYPE1, IRANK1>::type in1, 
	typename promote<ITYPE2, IRANK2>::type in2, 
	typename promote<ITYPE3, IRANK3>::type in3, 
	typename promote<ITYPE4, IRANK4>::type in4, 
	typename promote<ITYPE5, IRANK5>::type in5, 
	typename promote<ITYPE6, IRANK6>::type in6, 
	typename promote<ITYPE7, IRANK7>::type in7, 
	typename promote<ITYPE8, IRANK8>::type in8, 
	typename promote<OTYPE, ORANK>::type out, 
	const size_t in1_extents[IRANK1], 
	const size_t in2_extents[IRANK2], 
	const size_t in3_extents[IRANK3], 
	const size_t in4_extents[IRANK4], 
	const size_t in5_extents[IRANK5], 
	const size_t in6_extents[IRANK6], 
	const size_t in7_extents[IRANK7], 
	const size_t in8_extents[IRANK8],
	const size_t out_extents[ORANK]){
	// Nothing to see here.
}

template<> void add10<float, 1, nullptr, float, 1, nullptr, float, 1, nullptr, float, 1, nullptr, float, 1, nullptr, float, 1, nullptr, float, 1, nullptr, float, 1, nullptr, float, 8, res_symm>(
	typename promote<float, 1>::type in1, 
	typename promote<float, 1>::type in2, 
	typename promote<float, 1>::type in3, 
	typename promote<float, 1>::type in4, 
	typename promote<float, 1>::type in5, 
	typename promote<float, 1>::type in6, 
	typename promote<float, 1>::type in7, 
	typename promote<float, 1>::type in8, 
	typename promote<float, 8>::type out, 
	const size_t in1_extents[1], 
	const size_t in2_extents[1], 
	const size_t in3_extents[1], 
	const size_t in4_extents[1], 
	const size_t in5_extents[1], 
	const size_t in6_extents[1], 
	const size_t in7_extents[1], 
	const size_t in8_extents[1], 
	const size_t out_extents[8]) {
	size_t __i0 = 0;
	#pragma omp parallel for private(__i0)
	for(__i0 = 0; __i0 < in1_extents[0] - 0; __i0++) {
		promote<float, 0>::type in1__i0 = in1[__i0];
		size_t __i1 = 0;
		
		for(__i1 = 0; __i1 < in2_extents[0] - __i0; __i1++) {
			promote<float, 0>::type in2__i1 = in2[__i1];
			size_t __i2 = 0;
			
			for(__i2 = 0; __i2 < in3_extents[0] - __i1 - __i0; __i2++) {
				promote<float, 0>::type in3__i2 = in3[__i2];
				size_t __i3 = 0;
				
				for(__i3 = 0; __i3 < in4_extents[0] - __i2 - __i1 - __i0; __i3++) {
					promote<float, 0>::type in4__i3 = in4[__i3];
					size_t __i4 = 0;
					
					for(__i4 = 0; __i4 < in5_extents[0] - __i3 - __i2 - __i1 - __i0; __i4++) {
						promote<float, 0>::type in5__i4 = in5[__i4];
						size_t __i5 = 0;
						
						for(__i5 = 0; __i5 < in6_extents[0] - __i4 - __i3 - __i2 - __i1 - __i0; __i5++) {
							promote<float, 0>::type in6__i5 = in6[__i5];
							size_t __i6 = 0;
							
							for(__i6 = 0; __i6 < in7_extents[0] - __i5 - __i4 - __i3 - __i2 - __i1 - __i0; __i6++) {
								promote<float, 0>::type in7__i6 = in7[__i6];
								size_t __i7 = 0;
								
								for(__i7 = 0; __i7 < in8_extents[0] - 0; __i7++) {
									promote<float, 0>::type in8__i7 = in8[__i7];
									out[__i0][__i1][__i2][__i3][__i4][__i5][__i6][__i7]=in1__i0+in2__i1+in3__i2+in4__i3+in5__i4+in6__i5+in7__i6+in8__i7;
								}
							}
						}
					}
				}
			}
		}
	}
	}
static constexpr const size_t extents1[1]={16};
static constexpr const size_t extents0[0]={};
int main(){
auto start=TIME;


promote<float, 1>::type a1;
a1 = allocate<typename promote<float, 1>::type,nullptr>(extents1);


promote<float, 1>::type a2;
a2 = allocate<typename promote<float, 1>::type,nullptr>(extents1);


promote<float, 1>::type a3;
a3 = allocate<typename promote<float, 1>::type,nullptr>(extents1);


promote<float, 1>::type a4;
a4 = allocate<typename promote<float, 1>::type,nullptr>(extents1);


promote<float, 1>::type a5;
a5 = allocate<typename promote<float, 1>::type,nullptr>(extents1);


promote<float, 1>::type a6;
a6 = allocate<typename promote<float, 1>::type,nullptr>(extents1);


promote<float, 1>::type a7;
a7 = allocate<typename promote<float, 1>::type,nullptr>(extents1);


promote<float, 1>::type a8;
a8 = allocate<typename promote<float, 1>::type,nullptr>(extents1);
auto end=TIME;
double diff=1e-9*TIME_DIFF;
cout<<"Input Allocation took "<<diff<<"s"<<endl<<endl;
typedef promote<float,1>::type float1;
fill_random<float1>(a1,extents1,10);
fill_random<float1>(a2,extents1,10);
fill_random<float1>(a3,extents1,10);
fill_random<float1>(a4,extents1,10);
fill_random<float1>(a5,extents1,10);
fill_random<float1>(a6,extents1,10);
fill_random<float1>(a7,extents1,10);
fill_random<float1>(a8,extents1,10);
start=TIME;
size_t* extents0_res=new size_t[8];
extents0_res[0]=extents1[0];
extents0_res[1]=extents1[0];
extents0_res[2]=extents1[0];
extents0_res[3]=extents1[0];
extents0_res[4]=extents1[0];
extents0_res[5]=extents1[0];
extents0_res[6]=extents1[0];
extents0_res[7]=extents1[0];
promote<float,8>::type res;
res=allocate<typename promote<float,8>::type,res_symm>(extents0_res);
end=TIME;
diff=1e-9*TIME_DIFF;
cout<<"Output Allocation took "<<diff<<"s"<<endl<<endl;
start=TIME;
add10<float,1,nullptr,float,1,nullptr,float,1,nullptr,float,1,nullptr,float,1,nullptr,float,1,nullptr,float,1,nullptr,float,1,nullptr,float,8,res_symm>(a1,a1,a1,a1,a1,a1,a1,a8,res,extents1,extents1,extents1,extents1,extents1,extents1,extents1,extents1,extents0_res);
end=TIME;
diff=1e-9*TIME_DIFF;
cout<<"Calculation took "<<diff<<"s"<<endl<<endl;
return 0;
}


