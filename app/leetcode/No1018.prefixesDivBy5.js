/**
 * @param {number[]} A
 * @return {boolean[]}
 */
var prefixesDivBy5 = function (A) {
    // const res = [];
    // for (let i = 0; i < A.length; i++) {
    //     let radix = 0;
    //     let num = 0;
    //     for (let j = i; j >= 0; j--, radix++) {
    //         num += A[j] * (2 ** radix);
    //     }
    //     if (num % 5 === 0) {
    //         res.push(true);
    //     } else {
    //         res.push(false);
    //     }
    // }
    // return res;
    // 暴力法居然出错了，我他妈
    const list = [];
    let prefix = 0;
    for (const a of A) {
        prefix = ((prefix << 1) + a) % 5;
        list.push(prefix === 0);
    }
    return list;
};

A = [0, 1, 1, 1, 1, 1];
prefixesDivBy5(A);
debugger
