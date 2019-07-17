class Transaction {
  constructor(id, when, amount) {
    this.id = id;
    this.when = when;
    this.amount = amount;
  }

  compareTo(other) {
    return this.amount - other.amount;
  }
}

const swap = require('../../swap');
const {shuffle, randomInt, randomDate} = require('../../util');

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

  // shuffle(arr);
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

const sortFns = [insertSort, selectionSort, shellSort, mergeSort, quickSort, heapSort];
const Ns = [1e3, 1e4, 1e5, 1e6];

const T = 10;

function test(N) {
  const arr = [];
  const startDate = new Date(1993, 6, 19);
  const endDate = new Date();
  for (let i = 0; i < N; i++) {
    arr.push(new Transaction(randomInt(0, 1000000), randomDate(startDate, endDate), randomInt(0, 2123243543)));
  }
  for (const fn of sortFns) {
    let totalTime = 0;
    for (let i = 0; i < T; i++) {
      const sortArr = arr.slice();
      const startTime = Date.now();
      fn.call(null, sortArr);
      totalTime += (Date.now() - startTime);
    }
    console.log('N = ', N, 'Fn = ', fn.name, 'cost', totalTime / T);
  }
  console.log();
}

for (const n of Ns) {
  test(n);
}

// N =  1000 Fn =  insertSort cost 3.2
// N =  1000 Fn =  selectionSort cost 4.7
// N =  1000 Fn =  shellSort cost 0.9
// N =  1000 Fn =  mergeSort cost 2.2
// N =  1000 Fn =  quickSort cost 1.9
// N =  1000 Fn =  heapSort cost 3
//
// N =  10000 Fn =  insertSort cost 243.8
// N =  10000 Fn =  selectionSort cost 414.6
// N =  10000 Fn =  shellSort cost 4.4
// N =  10000 Fn =  mergeSort cost 6.2
// N =  10000 Fn =  quickSort cost 2.3
// N =  10000 Fn =  heapSort cost 13
//
// N =  100000 Fn =  insertSort cost 257551.5
// N =  100000 Fn =  selectionSort cost 77469.5
// N =  100000 Fn =  shellSort cost 314.6
// N =  100000 Fn =  mergeSort cost 94.3
// N =  100000 Fn =  quickSort cost 38.1
// N =  100000 Fn =  heapSort cost 251.5
//
// N =  1000000 Fn =  insertSort cost 25747413.9
// N =  1000000 Fn =  selectionSort cost 15947579.3
// N =  1000000 Fn =  shellSort cost 6182.1
// N =  1000000 Fn =  mergeSort cost 2093.9
// N =  1000000 Fn =  quickSort cost 1068.5
// N =  1000000 Fn =  heapSort cost 4394.9
