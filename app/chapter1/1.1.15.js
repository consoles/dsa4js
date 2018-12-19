// 接收一个整型数组arr和一个整数m
// 返回一个大小为m的数组，其中第i个元素的值为整数i在参数数组中出现的次数
// 如果arr[] 中的值均在0到m-1，则返回数组中所有元素的和应该和arr.length相等

const _ = require('lodash');

const histogram = (arr, m) => {
    const ret = new Array(m).fill(0);
    for (let num of arr) {
        ret[num]++;
    }
    return ret;
}

const m = 8;
const arr = [];
for (let i = 0; i < 9; i++) {
    arr.push(_.random(m - 1));
}

const ret = histogram(arr, m);
const sum = _.sum(ret);
debugger;