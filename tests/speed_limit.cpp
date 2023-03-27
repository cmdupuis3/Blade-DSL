
#include <iostream>
#include <cmath>
using namespace std;

const size_t LENGTH = 100;

size_t loop1() {
    size_t j=0;
    for (int i1 = 0; i1 < LENGTH; i1++) {
        j++;
    }
    return j;
}

size_t loop2() {
    size_t j=0;
    for (int i1 = 0; i1 < LENGTH; i1++) {
    for (int i2 = 0; i2 < LENGTH - i1; i2++) {
        j++;
    }
    }
    return j;
}

size_t loop3() {
    size_t j=0;
    for (int i1 = 0; i1 < LENGTH; i1++) {
    for (int i2 = 0; i2 < LENGTH - i1; i2++) {
    for (int i3 = 0; i3 < LENGTH - i2 - i1; i3++) {
        j++;
    }
    }
    }
    return j;
}

size_t loop4() {
    size_t j=0;
    for (int i1 = 0; i1 < LENGTH; i1++) {
    for (int i2 = 0; i2 < LENGTH - i1; i2++) {
    for (int i3 = 0; i3 < LENGTH - i2 - i1; i3++) {
    for (int i4 = 0; i4 < LENGTH - i3 - i2 - i1; i4++) {
        j++;
    }
    }
    }
    }
    return j;
}

size_t loop5() {
    size_t j=0;
    for (int i1 = 0; i1 < LENGTH; i1++) {
    for (int i2 = 0; i2 < LENGTH - i1; i2++) {
    for (int i3 = 0; i3 < LENGTH - i2 - i1; i3++) {
    for (int i4 = 0; i4 < LENGTH - i3 - i2 - i1; i4++) {
    for (int i5 = 0; i5 < LENGTH - i4 - i3 - i2 - i1; i5++) {
        j++;
    }
    }
    }
    }
    }
    return j;
}

size_t loop6() {
    size_t j=0;
    for (int i1 = 0; i1 < LENGTH; i1++) {
    for (int i2 = 0; i2 < LENGTH - i1; i2++) {
    for (int i3 = 0; i3 < LENGTH - i2 - i1; i3++) {
    for (int i4 = 0; i4 < LENGTH - i3 - i2 - i1; i4++) {
    for (int i5 = 0; i5 < LENGTH - i4 - i3 - i2 - i1; i5++) {
    for (int i6 = 0; i6 < LENGTH - i5 - i4 - i3 - i2 - i1; i6++) {
        j++;
    }
    }
    }
    }
    }
    }
    return j;
}

size_t loop7() {
    size_t j=0;
    for (int i1 = 0; i1 < LENGTH; i1++) {
    for (int i2 = 0; i2 < LENGTH - i1; i2++) {
    for (int i3 = 0; i3 < LENGTH - i2 - i1; i3++) {
    for (int i4 = 0; i4 < LENGTH - i3 - i2 - i1; i4++) {
    for (int i5 = 0; i5 < LENGTH - i4 - i3 - i2 - i1; i5++) {
    for (int i6 = 0; i6 < LENGTH - i5 - i4 - i3 - i2 - i1; i6++) {
    for (int i7 = 0; i7 < LENGTH - i6 - i5 - i4 - i3 - i2 - i1; i7++) {
        j++;
    }
    }
    }
    }
    }
    }
    }
    return j;
}

size_t loop8() {
    size_t j=0;
    for (int i1 = 0; i1 < LENGTH; i1++) {
    for (int i2 = 0; i2 < LENGTH - i1; i2++) {
    for (int i3 = 0; i3 < LENGTH - i2 - i1; i3++) {
    for (int i4 = 0; i4 < LENGTH - i3 - i2 - i1; i4++) {
    for (int i5 = 0; i5 < LENGTH - i4 - i3 - i2 - i1; i5++) {
    for (int i6 = 0; i6 < LENGTH - i5 - i4 - i3 - i2 - i1; i6++) {
    for (int i7 = 0; i7 < LENGTH - i6 - i5 - i4 - i3 - i2 - i1; i7++) {
    for (int i8 = 0; i8 < LENGTH - i7 - i6 - i5 - i4 - i3 - i2 - i1; i8++) {
        j++;
    }
    }
    }
    }
    }
    }
    }
    }
    return j;
}


int main() {
    typedef size_t (*loop_t)();

    loop_t loops[10] = { loop1, loop2, loop3, loop4, loop5,
                         loop6, loop7, loop8/*, loop9, loop10*/ };

    for(int k = 0; k < 8; k++) {
        cout << (double)loops[k]() / (double)(pow(LENGTH,k+1)) << endl; 
    }

    return 0;

}
