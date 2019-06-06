// 三取样切分

// 为快排实现2.3.3.2中所述的三取样切分，运行双倍测试来确认这项改动的效果

const assert = require('assert');

const {shuffle, isSorted} = require('../../util');
const swap = require('../../swap');
const {sortCompare, quickSort} = require('../../sort');

function partition(arr, lo, hi) {
  let i = lo, j = hi + 1;
  if (arr[lo + 1] < arr[lo]) {
    swap(arr, lo + 1, lo);
  }
  if (arr[lo + 2] < arr[lo]) {
    swap(arr, lo + 2, lo);
  }
  // lo最小,放在最前面了，接下来比较arr[lo+1]&arr[lo+2]
  if (arr[lo + 2] < arr[lo + 1]) {
    swap(arr, lo + 2, lo + 1);
  }
  // arr[lo],arr[lo+1],arr[lo+2]从小到大排列了
  swap(arr, lo, lo + 1); // 中位数放左侧
  swap(arr, hi, lo + 2); // 较大的值放在最右侧作为哨兵
  const v = arr[lo];
  while (true) {
    while (arr[++i] < v) ;
    while (arr[--j] > v) ;
    if (i >= j) break;
    swap(arr, i, j);
  }
  swap(arr, lo, j);
  return j;
}

function _quickSort(arr, lo, hi) {
  if (hi <= lo) return;
  // 两个数组的元素直接排序，因为partition寻找中位数的过程中数组长度至少为3
  if (hi === lo + 1) {
    if (arr[hi] < arr[lo]) {
      swap(arr, lo, hi);
    }
    return;
  }
  const j = partition(arr, lo, hi);
  _quickSort(arr, lo, j - 1);
  _quickSort(arr, j + 1, hi);
}

function quickSort3Partition(arr) {
  shuffle(arr);
  _quickSort(arr, 0, arr.length - 1);
  assert(isSorted(arr));
}

let n = 1000;
while (true) {
  const ret = sortCompare(n, 10, quickSort, quickSort3Partition);
  console.log('n = ', n, ret);
  n *= 2;
}

// n =  1000 { quickSort: 4, quickSort3Partition: 1.8 }
// n =  2000 { quickSort: 3.8, quickSort3Partition: 3.7 }
// n =  4000 { quickSort: 7, quickSort3Partition: 6.9 }
// n =  8000 { quickSort: 13.6, quickSort3Partition: 14.1 }
// n =  16000 { quickSort: 28, quickSort3Partition: 29.7 }
// n =  32000 { quickSort: 57.1, quickSort3Partition: 55 }
// n =  64000 { quickSort: 111.9, quickSort3Partition: 113.3 }
// n =  128000 { quickSort: 232.2, quickSort3Partition: 227 }
// n =  256000 { quickSort: 450.8, quickSort3Partition: 451.3 }
// n =  512000 { quickSort: 917.1, quickSort3Partition: 926.3 }
