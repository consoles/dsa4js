// 在使用快排将N个不重复的元素排序的时候，计算大小为0,1,2的子数组的数量。
// 如果你喜欢数学，请推导；如果你不喜欢，请做一些实验并提出猜想

const {shuffle, randomDoubleArray} = require('../../util');
const swap = require('../../swap');

class QuickSortAnalyze {
  constructor(arr) {
    this.arr = shuffle(arr);
    this.array0Num = 0;
    this.array1Num = 0;
    this.array2Num = 0;
  }

  _partition(lo, hi) {
    const v = this.arr[lo];
    let i = lo, j = hi + 1;
    while (true) {
      while (this.arr[++i] < v) {
        if (i === hi) {
          break;
        }
      }
      while (v < this.arr[--j]) {
        if (j === lo) {
          break;
        }
      }
      if (i >= j) {
        break;
      }
      swap(this.arr, i, j);
    }
    swap(this.arr, lo, j);
    return j;
  }

  _quickSort(lo, hi) {
    const len = hi - lo + 1;
    if (len === 2) {
      this.array2Num++;
    } else if (len === 1) {
      this.array1Num++;
    } else if (len === 0) {
      this.array0Num++;
    }
    if (lo >= hi) return;
    const j = this._partition(lo, hi);
    this._quickSort(lo, j - 1);
    this._quickSort(j + 1, hi);
  }

  sort() {
    this._quickSort(0, this.arr.length - 1);
  }
}

const nums = [1000, 2000, 4000, 8000,16000];
const TEST_COUNT = 20;

for (const n of nums) {
  const counts = [0, 0, 0];
  for (let i = 0; i < TEST_COUNT; i++) {
    const arr = randomDoubleArray(n);
    const s = new QuickSortAnalyze(arr);
    s.sort();
    counts[0] += s.array0Num;
    counts[1] += s.array1Num;
    counts[2] += s.array2Num;
  }
  console.log('n = ', n, '0子数组', counts[0] / TEST_COUNT, '1子数组', counts[1] / TEST_COUNT, '2子数组', counts[2] / TEST_COUNT);
}

// n =  1000 0子数组 331.7 1子数组 334.65 2子数组 165.05
// n =  2000 0子数组 659.5 1子数组 670.75 2子数组 332.15
// n =  4000 0子数组 1330.3 1子数组 1335.35 2子数组 665.9
// n =  8000 0子数组 2677.6 1子数组 2661.7 2子数组 1333.55

// 猜想c(0) = c(1) = n / 3,c(2) = n / 6
