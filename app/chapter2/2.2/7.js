// 证明归并排序的比较次数是单调递增的，即对于N > 0,C(N+1) > C(N)

// 根据书中给出的命题G和命题H，比较次数的下限是C(N) = 1/2 * NlgN
// N 和lgN都是单调递增，并且N > 1，所以C(N)单调递增