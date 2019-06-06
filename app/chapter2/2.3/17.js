// 哨兵

// 修改算法2.5去掉内循环while中的边界检查。由于切分元素本身就是一个哨兵v不可能小于arr[lo],左侧边界的检查是多余的。要去掉另一个检查，可以在打乱数组后将数组最大元素放在arr[length-1]中，该元素永远不会移动（除非和相等的元素交换），可以在所有包含它的子数组中成为哨兵。注意：在处理内部数组时，右侧数组中最左侧的元素可以作为子数组中右边界的哨兵。

const {shuffle} = require('../../util');
const swap = require('../../swap');

function partition(arr, lo, hi) {
  let i = lo, j = hi + 1;
  const v = arr[lo];
  while (true) {
    while (arr[++i] < v) {
      // if (i === hi) {
      //   break;
      // }
    }
    while (arr[--j] > v) {
      // if (j === lo) {
      //   break;
      // }
    }
    if (i >= j) {
      break;
    }
    swap(arr, i, j);
  }
  swap(arr, j, lo);
  return j;
}

function _quickSort(arr, lo, hi) {
  if (hi <= lo) return;
  const j = partition(arr, lo, hi);
  _quickSort(arr, lo, j - 1);
  _quickSort(arr, j + 1, hi);
}

function quickSort(arr) {
  shuffle(arr);
  // 最大元素放到最后一位
  let maxIndex = 0;
  for (let i = 1; i < arr.length; i++) {
    if (arr[i] < arr[maxIndex]) {
      maxIndex = i;
    }
  }
  if (maxIndex !== 0) {
    swap(arr, maxIndex, arr.length - 1);
  }
  _quickSort(arr, 0, arr.length - 1);
}

const arr = [2, 1, 4, 3, 5, 7, 0, 6];
quickSort(arr);
debugger;
