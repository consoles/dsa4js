class Item {
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

function test(sortFn, n, value) {
  const items = new Array(n).fill(value).map((value, index) => new Item(index, value));
  sortFn.call(null, items);
  console.log(sortFn.name, items.map(x => x.index));
}

const n = 7;
const value = 1;
const sortFns = [insertSort, selectionSort, shellSort, mergeSort, quickSort, heapSort];
for (const fn of sortFns) {
  test(fn, n, value);
}

// insertSort [ 0, 1, 2, 3, 4, 5, 6 ]
// selectionSort [ 0, 1, 2, 3, 4, 5, 6 ]
// shellSort [ 0, 1, 2, 3, 4, 5, 6 ]
// mergeSort [ 0, 1, 2, 3, 4, 5, 6 ]
// quickSort [ 3, 6, 2, 5, 4, 0, 1 ]
// heapSort [ 1, 2, 3, 4, 5, 6, 0 ]
// 只有快速排序和堆排序会进行交换，剩下四种排序都不会进行交换。
// 插入排序在排序元素完全相同的数组时只会进行一次遍历，不会交换。
// 选择排序第 i 次找到的最小值就是 a[i] ，只会让 a[i] 和 a[i] 交换，不会影响顺序。
// 希尔排序和插入排序类似，每轮排序都不会进行交换。
// 归并排序是稳定的，就本例而言，只会从左到右依次归并，不会发生顺序变化。
// 快速排序在遇到相同元素时会交换，因此顺序会发生变化，且每次都是对半切分。
// 堆排序在删除最大元素时会将第一个元素和最后一个元素交换，使元素顺序发生变化。
