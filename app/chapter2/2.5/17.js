// 用DataWrapper类包装准备排序的元素，在排序的同时记录下元素的内容和下标
// 随后对DataWrapper数组排序，相同的元素会被放在一起，检查下它们的下标是否是递增的
// 如果不是递增的，则算法是不稳定的；否则排序算法<可能是稳定的>（不稳定的排序算法也可能不改变元素的相对位置，例如用选择排序对有序数组进行排序）

class DataWrapper {
  constructor(index, value) {
    this.index = index;
    this.value = value;
  }

  compareTo(other) {
    return this.value - other.value;
  }
}

const swap = require('../../swap');
const {shuffle} = require('../../util');

function insertSort(arr) {
  const n = arr.length;
  for (let i = 1; i < n; i++) {
    for (let j = i; j > 0 && arr[j].compareTo(arr[j - 1]) < 0; j--) {
      swap(arr, j, j - 1);
    }
  }
}

function selectionSort(arr) {
  const n = arr.length;
  for (let i = 0; i < n; i++) {
    let minIndex = i;
    for (let j = i + 1; j < n; j++) {
      if (arr[minIndex].compareTo(arr[j]) > 0) {
        minIndex = j;
      }
    }
    if (minIndex !== i) {
      swap(arr, minIndex, i);
    }
  }
}

function shellSort(arr) {
  const n = arr.length;
  for (let gap = n >> 1; gap > 0; gap >>= 1) {
    for (let i = gap; i < n; i++) {
      for (let j = i; j >= gap && arr[j].compareTo(arr[j - gap]) < 0; j -= gap) {
        swap(arr, j, j - gap);
      }
    }
  }
}

function mergeSort(arr) {
  function _merge(arr, lo, mid, hi) {
    const aux = [];
    let i = lo, j = mid + 1;
    for (let k = lo; k <= hi; k++) {
      if (i > mid) {
        aux.push(arr[j++]);
      } else if (j > hi) {
        aux.push(arr[i++]);
      } else if (arr[i].compareTo(arr[j]) > 0) {
        aux.push(arr[j++]);
      } else {
        aux.push(arr[i++]);
      }
    }
    for (let i = 0; i < aux.length; i++) {
      arr[lo + i] = aux[i];
    }
  }

  function _mergeSort(arr, lo, hi) {
    if (lo >= hi) return;
    const mid = lo + parseInt((hi - lo) / 2);
    _mergeSort(arr, lo, mid);
    _mergeSort(arr, mid + 1, hi);
    _merge(arr, lo, mid, hi);
  }

  _mergeSort(arr, 0, arr.length - 1);
}

function quickSort(arr) {
  function _quickSort(arr, lo, hi) {
    if (lo >= hi) return;
    const j = _partition(arr, lo, hi);
    _quickSort(arr, lo, j - 1);
    _quickSort(arr, j + 1, hi);
  }

  function _partition(arr, lo, hi) {
    let i = lo;
    let j = hi + 1;
    const v = arr[lo];
    while (true) {
      while (arr[++i].compareTo(v) < 0) {
        if (i === hi) {
          break;
        }
      }
      while (arr[--j].compareTo(v) > 0) {
        if (j === lo) {
          break;
        }
      }
      if (i >= j) {
        break;
      }
      swap(arr, i, j);
    }
    swap(arr, lo, j);
    return j;
  }

  shuffle(arr);
  _quickSort(arr, 0, arr.length - 1);
}

function heapSort(arr) {
  function _less(arr, i, j) {
    return arr[i - 1].compareTo(arr[j - 1]) < 0;
  }

  function _swap(arr, i, j) {
    [arr[i - 1], arr[j - 1]] = [arr[j - 1], arr[i - 1]];
  }

  function _sink(arr, k, n) {
    while (2 * k <= n) {
      let j = 2 * k;
      if (j < n && _less(arr, j, j + 1)) {
        j++;
      }
      if (_less(arr, k, j)) {
        _swap(arr, j, k);
        k = j;
      } else {
        break;
      }
    }
  }

  let n = arr.length;
  for (let k = parseInt(n / 2); k >= 1; k--) {
    _sink(arr, k, n);
  }
  while (n > 1) {
    _swap(arr, 1, n--);
    _sink(arr, 1, n);
  }
}

const sortMethods = [selectionSort, insertSort, shellSort, mergeSort, quickSort, heapSort];

function checkSortStability(sortFn) {
  const wrapDatas = [7, 7, 4, 8, 8, 5, 1, 7, 7].map((value, index) => new DataWrapper(index, value));
  sortFn.call(null, wrapDatas);
  for (let i = 0; i < wrapDatas.length - 1; i++) {
    if (wrapDatas[i].value === wrapDatas[i + 1].value && wrapDatas[i].index > wrapDatas[i + 1].index) {
      console.log(sortFn.name, '是不稳定排序');
      return;
    }
  }
  console.log(sortFn.name, '可能是稳定排序');
}

for (const sortFn of sortMethods) {
  checkSortStability(sortFn);
}
