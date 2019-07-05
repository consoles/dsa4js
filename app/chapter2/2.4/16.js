// 对于N=32，构造数组使得堆排序使用的比较次数最多以及最少

const swap = require('../../swap');

let count = 0;

function less(arr, i, j) {
  count++;
  return arr[i] < arr[j];
}

/**
 * 将arr中索引为k位置的元素，下沉到最大索引n-1
 * @param arr
 * @param k
 * @param n
 */
function sink(arr, k, n) {
  while (k < n) {
    let j = 2 * k + 1;
    if (j >= n) {
      break;
    }
    if (j + 1 < n && !less(arr, j + 1, j)) {
      j++;
    }
    if (!less(arr, k, j)) {
      break;
    }
    swap(arr, k, j);
    k = j;
  }
}

function heapSort(arr) {
  let n = arr.length;
  for (let k = parseInt(n / 2); k >= 0; k--) {
    sink(arr, k, n);
  }
  while (n > 0) {
    swap(arr, 0, --n);
    sink(arr, 0, n);
  }
}

// 最好情况，数组元素全部相等,90次比较
let arr = new Array(32).fill(0);
heapSort(arr);
debugger;

// 最坏情况247次比较
count = 0;
arr = [1, 4, 7, 12, 10, 16, 14, 19, 17, 20, 5, 27, 8, 28, 2, 24, 9, 18, 6, 23, 11, 22, 21, 31, 13, 26, 25, 30, 15, 29, 3, 32];
heapSort(arr);
debugger;

// 如何构造使得基于堆排序的比较次数最多的堆
