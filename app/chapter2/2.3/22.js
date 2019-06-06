// 快速三向切分
// 用将重复元素放置于数组两端的方式实现一个信息量最优的排序算法。使用两个索引p和q，使得arr[lo...p-1]和arr[q+1...hi]的元素都和arr[lo]相等。使用另外2个索引i和j，使得arr[p...i-1]小于arr[lo],arr[j+1...q]大于arr[lo]。在内循环中加入代码，在arr[i]和v相当时将其与arr[p]交换（并将p加1），在arr[j]和v相等且arr[i]和arr[j]尚未和v进行比较之前将其和arr[q]交换。添加在切分循环后将将和v相等的元素交换到正确位置的代码，这里额外的交换用于和切分元素相等的元素，而正文中的代码将额外的交换用于和切分元素不等的元素

const assert = require('assert');
const swap = require('../../swap');
const {shuffle, isSorted} = require('../../util');
const {sortCompare, quickSort} = require('../../sort');

function insertSort(arr, lo, hi) {
  for (let i = lo; i <= hi; i++) {
    for (let j = i; j > 0 && arr[j] < arr[j - 1]; j--) {
      swap(arr, j, j - 1);
    }
  }
}

const INSERTION_SORT_CUTOFF = 8; // 小于这个值采用插入排序
const MEDIAN_OF_3_CUTOFF = 40; // 小于这个值用数组中位数作为枢轴

/**
 * 求三个元素的中位数
 */
function median3(arr, i, j, k) {
  return arr[i] < arr[j] ?
    (arr[j] < arr[k] ? j : arr[i] < arr[k] ? k : i) :
    (arr[k] < arr[j] ? j : arr[k] < arr[i] ? k : i);
}

function _quickSort(arr, lo, hi) {
  if (hi <= lo) return;
  const n = hi - lo + 1;
  if (n <= INSERTION_SORT_CUTOFF) {
    insertSort(arr, lo, hi);
    return;
  }
  if (n <= MEDIAN_OF_3_CUTOFF) {
    const m = median3(arr, lo, lo + parseInt(n / 2), hi); // 对于较小的数组，直接选择左中右三个元素中的中位数作为枢轴
    swap(arr, m, lo);
  } else {
    const eps = parseInt(n / 8);
    const mid = lo + parseInt(n / 2);
    const m1 = median3(arr, lo, lo + eps, lo + 2 * eps);
    const m2 = median3(arr, mid - eps, mid, mid + eps);
    const m3 = median3(arr, hi - 2 * eps, hi - eps, hi);
    const ninther = median3(arr, m1, m2, m3); // 对于较大的数组使用 Turkey Ninther 作为枢轴。
    swap(arr, ninther, lo);
  }
  // 三向切分
  let i = lo, j = hi + 1;
  let p = lo, q = hi + 1;
  const v = arr[lo];
  while (true) {
    while (arr[++i] < v) if (i === hi) break;
    while (arr[--j] > v) if (j === lo) break;
    if (i === j && arr[i] === v) swap(arr, ++p, i);
    if (i >= j) break;
    swap(arr, i, j);
    if (arr[i] === v) swap(arr, ++p, i);
    if (arr[j] === v) swap(arr, --q, j);
  }
  i = j + 1;
  for (let k = lo; k <= p; k++) {
    swap(arr, k, j--);
  }
  for (let k = hi; k >= q; k--) {
    swap(arr, k, i++);
  }
  _quickSort(arr, lo, j);
  _quickSort(arr, i, hi);
}

function quickSort3Ways(arr) {
  _quickSort(arr, 0, arr.length - 1);
  assert(isSorted(arr));
}

let n = 1000;
while (true) {
  const ret = sortCompare(n, 10, quickSort, quickSort3Ways);
  console.log('n = ', n, ret);
  n *= 2;
}

// n =  1000 { quickSort: 4.4, quickSort3Ways: 2.1 }
// n =  2000 { quickSort: 3.7, quickSort3Ways: 3.7 }
// n =  4000 { quickSort: 7.6, quickSort3Ways: 6.9 }
// n =  8000 { quickSort: 13.3, quickSort3Ways: 14.2 }
// n =  16000 { quickSort: 28.5, quickSort3Ways: 29 }
// n =  32000 { quickSort: 55.6, quickSort3Ways: 54.2 }
// n =  64000 { quickSort: 107.8, quickSort3Ways: 111.6 }
// n =  128000 { quickSort: 228.4, quickSort3Ways: 221.6 }
// n =  256000 { quickSort: 440.5, quickSort3Ways: 447.3 }
// n =  512000 { quickSort: 905, quickSort3Ways: 916.3 }
// n =  1024000 { quickSort: 1802.2, quickSort3Ways: 1831.3 }
