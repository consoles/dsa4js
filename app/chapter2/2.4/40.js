class HeapSort {

  constructor() {
    this.compareCount = 0;
  }

  sort(arr) {
    let n = arr.length;
    // 建堆
    for (let k = parseInt(n / 2); k >= 1; k--) {
      this._sink(arr, k, n);
    }
    while (n > 1) {
      this._swap(arr, 1, n--);
      this._sink(arr, 1, n);
    }
  }

  _sink(arr, k, n) {
    while (2 * k <= n) {
      let j = 2 * k;
      if (j < n && this._less(arr, j, j + 1)) {
        j++;
      }
      if (this._less(arr, k, j)) {
        this._swap(arr, j, k);
        k = j;
      } else {
        break;
      }
    }
  }

  _less(arr, i, j) {
    this.compareCount++;
    return arr[i - 1] < arr[j - 1];
  }

  _swap(arr, i, j) {
    [arr[i - 1], arr[j - 1]] = [arr[j - 1], arr[i - 1]];
  }
}

class HeapSortFloyd extends HeapSort {
  sort(arr) {
    let n = arr.length;
    // 建堆
    for (let k = parseInt(n / 2); k >= 1; k--) {
      this._sink(arr, k, n);
    }
    // 排序
    while (n > 1) {
      this._swap(arr, 1, n--);
      this._sinkThenSwim(arr, 1, n);
    }
  }

  _swim(arr, k) {
    while (k > 1) {
      const parentIndex = parseInt(k / 2);
      if (this._less(arr,parentIndex,k)) {
        this._swap(arr,k,parentIndex);
        k = parentIndex;
      } else{
        break;
      }
    }
  }

  _sinkThenSwim(arr, k, n) {
    while (2 * k <= n) {
      let j = 2 * k;
      if (j < n && this._less(arr, j, j + 1)) {
        j++;
      }
      // 去掉下沉的条件this._less(arr, k, j)
      this._swap(arr, k, j);
      k = j;
    }
    // sink循环结束之后调用swim
    this._swim(arr, k);
  }
}

const assert = require('assert');
const {genRandomDoubleArray, isSorted} = require('../../util');

const testCount = 10;
for (let n = 1e3; n < 1e7; n *= 10) {
  let totalTimeHeapSort = 0;
  let totalTimeHeapSortFloyd = 0;
  let compareCountHeapSort = 0;
  let compareCountHeapSortFloyd = 0;
  for (let i = 0; i < testCount; i++) {
    const arr1 = genRandomDoubleArray(n);
    const arr2 = arr1.slice();
    const s1 = new HeapSort();
    const s2 = new HeapSortFloyd();
    const t1Start = Date.now();
    s1.sort(arr1);
    const t1End = Date.now();
    s2.sort(arr2);
    const t2End = Date.now();
    totalTimeHeapSort += (t1End - t1Start);
    totalTimeHeapSortFloyd += (t2End - t1End);
    compareCountHeapSort += s1.compareCount;
    compareCountHeapSortFloyd += s2.compareCount;
    assert(isSorted(arr1) && isSorted(arr2));
  }
  console.log('数据规模 n = ', n, 'heapSort cost', totalTimeHeapSort / testCount, 'heapSort compare count', compareCountHeapSort / testCount, 'heapSortFloyd cost', totalTimeHeapSortFloyd / testCount, 'heapSortFloyd compare count', compareCountHeapSortFloyd / testCount);
}

// 数据规模 n =  1000 heapSort cost 1.6 heapSort compare count 16852.4 heapSortFloyd cost 0.8 heapSortFloyd compare count 10532
// 数据规模 n =  10000 heapSort cost 4 heapSort compare count 235373.5 heapSortFloyd cost 3.5 heapSortFloyd compare count 138963.1
// 数据规模 n =  100000 heapSort cost 48.5 heapSort compare count 3019546.8 heapSortFloyd cost 45.5 heapSortFloyd compare count 1722768.8
// 数据规模 n =  1000000 heapSort cost 596.7 heapSort compare count 36793764.3 heapSortFloyd cost 579.4 heapSortFloyd compare count 20526764.1
// 可以节约50%的比较次数
