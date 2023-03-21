

#include <iostream>
using std::cout;
using std::endl;

#include "nested_array_utilities.cpp"
using namespace nested_array_utilities;

void test_partial_set_index() {

    typedef typename promote<size_t, 6>::type size_t6;
    static constexpr const size_t ndims = get_rank<size_t6>();
    static constexpr const size_t symms[6] = { 1, 1, 1, 4, 5, 5 };
    const size_t inds[5] = { 2, 3, 9, 0, 2 };
    constexpr const size_t ninds = extent<decltype(inds)>::value;

    const size_t exts[6] = { 10, 10, 10, 10, 10, 10 };
    size_t6 arr = allocate<size_t6, symms>(exts);

    size_t* derp = new size_t[5];
    for (size_t i = 0; i < 5; i++) {
        derp[i] = i;
    }

    set_index<size_t6, symms, size_t*>(arr, inds, derp);
    for (size_t i = 0; i < 5; i++) {
        cout << arr[inds[0]][inds[1] - inds[0]][inds[2] - inds[1]][inds[3]][inds[4]][i] << endl;
    }

    return;
}

void index_test() {

    const size_t ndims = 6;
    const size_t inds[6] = { 2, 3, 9, 0, 1, 9 };
    const size_t inds2[6] = { 9, 3, 2, 0, 9, 1 };
    static constexpr const size_t symms[6] = { 1, 1, 1, 4, 5, 5 };
    constexpr const size_t nsymms = extent<decltype(symms)>::value;

    typedef typename promote<size_t, 6>::type size_t6;
    const size_t exts[6] = { 10, 10, 10, 10, 10, 10 };
    size_t6 arr = allocate<size_t6, symms>(exts);

    arr[inds[0]][inds[1] - inds[0]][inds[2] - inds[1]][inds[3]][inds[4]][inds[5] - inds[4]] = 20;
    cout << arr[inds[0]][inds[1] - inds[0]][inds[2] - inds[1]][inds[3]][inds[4]][inds[5] - inds[4]] << endl;

    set_index<size_t6, symms, size_t>(arr, inds, 30);
    cout << arr[inds[0]][inds[1] - inds[0]][inds[2] - inds[1]][inds[3]][inds[4]][inds[5] - inds[4]] << endl;

    auto a = index<size_t6, symms, nsymms, ndims>(arr, inds2);
    cout << a << endl;

    size_t* inds_folded = index_impl<ndims, symms>(inds);

    for (size_t i = 0; i < ndims; i++) {
        cout << inds[i] << "\t";
    }
    cout << endl;
    for (size_t i = 0; i < ndims; i++) {
        cout << inds_folded[i] << "\t";
    }
    cout << endl;


    size_t* inds_justified = ljustify<ndims, symms>(inds_folded);
    for (size_t i = 0; i < ndims; i++) {
        cout << inds_justified[i] << "\t";
    }

    return;
}