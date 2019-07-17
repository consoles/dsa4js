const swap = require('../../swap');

function shuffle(arr) {
  let j = arr.length;
  while (j--) {
    // len位置的元素和[0,len)的元素交换
    const i = Math.floor(Math.random() * j);
    swap(arr, i, j);
  }
  return arr;
}

function _partition(arr, lo, hi) {
  const v = arr[lo];
  let i = lo;
  let j = hi + 1;
  while (true) {
    while (arr[++i] < v) {
      if (i === hi) break;
    }
    while (arr[--j] > v) {
      if (j === lo) break;
    }
    if (i >= j) break;
    swap(arr, i, j);
  }
  swap(arr, j, lo);
  return j;
}

function _quickSort(arr, lo, hi) {
  if (lo >= hi) return;
  const j = _partition(arr, lo, hi);
  _quickSort(arr, lo, j - 1);
  _quickSort(arr, j + 1, hi);
}

function quickSort(arr) {
  shuffle(arr);
  _quickSort(arr, 0, arr.length - 1);
}

// 返回一个有序arr，并去除重复元素
// 排序nlogn
// 去重n，一共nlogn
// 空间复杂度n
function dedup(arr) {
  // 排序
  quickSort(arr);
  // 去重
  const ret = [];
  for (const item of arr) {
    if (item !== ret[ret.length - 1]) {
      ret.push(item);
    }
  }
  return ret;
}

const arr = [1, 5, 3, 1, 5, 3, 2, 7, 2, 7, 9, 0, 5, 4, 3];
const ret = dedup(arr);
debugger;
