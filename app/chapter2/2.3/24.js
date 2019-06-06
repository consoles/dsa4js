// 取样排序
// 实现一个快排，取样大小为2^k-1，首先将取样得到的元素排序，然后在递归函数中使用样品的中位数切分。分为2部分的其余样品元素无需再次排序并可以分别应用于原数组的两个子数组。这种算法称为取样排序

// 取样排序的想法很简单：
// 常规快排的枢轴只有一个。
// 如果用一个数组来充当枢轴，根据排序位置的不同自动选择对应的枢轴，
// 显然能够更好的估计中位数，以求更好的切分效果。
// 于是引入了「取样」的概念，假如我们从源数组中随机取了 3 个元素并对其排序，
// 那么这 3 个元素的中位数可以作为第一次切分的枢轴，剩余两个元素则可以充当切分后两个子数组的枢轴。
// 那么当取样元素到达一个合适的数量时，就能达到提升切分效率的目标。
//
// 大致思路如下：
// 首先先从输入数组里随机取一些元素，作为「取样数组」。
// 用任意排序算法（比如快排）对取样数组进行排序。
// （由于取样数组通常都比较小，这一步的时间消耗通常不会影响性能）
// 取出取样数组里面的中位数，当作枢轴对剩下的数组进行切分。
// 之后的切分中，根据排序区间在剩余数组中的相对位置，
// 用取样数组中对应位置的数作为枢轴，直到整个排序完成。
//
// 论文里提到了两种实现方式。
// 第一种方法
// 取样数组和剩余数组是分开保存的。
// 每次切分完成后，并不把枢轴放入剩余数组中，
// 而是等到剩余数组全部排序完毕之后再用一次归并（merge）操作将取样数组和剩余数组归并。
// 第二种方法
// 取样数组和剩余数组保存在同一片空间里，这也是这份题解所实现的方法。

const assert = require('assert');
const swap = require('../../swap');
const {shuffle, isSorted} = require('../../util');
const {quickSortWithRange} = require('../../sort');

function partition(arr, lo, hi) {
  let i = lo, j = hi + 1;
  const v = arr[lo];
  while (true) {
    while (arr[++i] < v) if (i === hi) break;
    while (arr[--j] > v) if (j === lo) break;
    if (i >= j) break;
    swap(arr, i, j);
  }
  swap(arr, j, lo);
  return j;
}

function sort(arr, sample_lo, lo, hi, sample_hi) {
  if (hi <= lo) {
    return;
  }
  const j = partition(arr, lo, hi);
  // 将前部的有序取样数组取半，后半部分放在枢轴前面
  if (lo - sample_lo > 1) {
    // p指向有序数组最后一项，v指向有序数组前面一项
    let p = lo - 1, v = j - 1;
    for (let i = 0; i < parseInt((lo - sample_lo) / 2); i++) {
      swap(arr, p--, v--);
    }
    sort(arr, sample_lo, p, v, j - 1);
  } else {
    // 取样数组已经用完，退化为普通quick sort
    quickSortWithRange(arr, sample_lo, j - 1);
  }

  // 将尾部有序取样数组取半，前半部分放在枢轴后面
  if (sample_hi - hi > 1) {
    // p 应该始终指向有序部分的前面一项, v 应该始终指向有序部分的最后一项
    let p = hi, v = j;
    for (let i = 0; i < parseInt((sample_hi - hi) / 2); i++) {
      swap(arr, ++p, ++v);
    }
    sort(arr, j + 1, v, p, sample_hi);
  } else {
    // 取样数组已经用完，退化为普通quick sort
    quickSortWithRange(arr, j + 1, sample_hi);
  }
}

function sampleSort(arr, k) {
  // 小于2^(k+1)的数组直接进行快排
  if (arr.length < 2 ** (k + 1)) {
    quickSortWithRange(arr, 0, arr.length - 1);
    return;
  }
  shuffle(arr);
  const sample_hi = 2 ** k - 2;
  // 利用快排对取样数组进行排序
  quickSortWithRange(arr, 0, sample_hi);
  // 取样数组的中位数
  const sampleMedian = parseInt(sample_hi / 2);
  // 将取样数组的后半部分放到数组末尾
  let i = sample_hi, j = arr.length - 1;
  while (i !== sampleMedian) {
    swap(arr, i--, j--);
  }
  // 根据取样数组进行排序
  sort(arr, 0, sampleMedian, j, arr.length - 1);
  assert(isSorted(arr));
}

const arr = [4,2,1,3,6,7,5,9,0,8];
sampleSort(arr,3);
debugger;

// let n = 1000;
// let last = 0;
// while (true) {
//   const arr = randomDoubleArray(n);
//   const start = Date.now();
//   sampleSort(arr, 8);
//   const time = Date.now() - start;
//   console.log('n = ', n, 'time = ', time, 'rate = ', time / last);
//   last = time;
//   n *= 2;
// }

// n =  1000 time =  12 rate =  Infinity
// n =  2000 time =  13 rate =  1.0833333333333333
// n =  4000 time =  15 rate =  1.1538461538461537
// n =  8000 time =  20 rate =  1.3333333333333333
// n =  16000 time =  26 rate =  1.3
// n =  32000 time =  58 rate =  2.230769230769231
// n =  64000 time =  122 rate =  2.103448275862069
// n =  128000 time =  238 rate =  1.9508196721311475
// n =  256000 time =  455 rate =  1.911764705882353
// n =  512000 time =  901 rate =  1.9802197802197803
// n =  1024000 time =  1820 rate =  2.019977802441731
