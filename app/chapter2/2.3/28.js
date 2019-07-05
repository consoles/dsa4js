// 递归深度

// 用经验性的研究估计切换阈值为M的快排在将大小为N的不重复数组排序时的平均递归深度，其中M=10，20和50,N=10^3,10^4,10^5,10^6

const assert = require('assert');

const swap = require('../../swap');
const {shuffle, isSorted, genRandomDoubleArray} = require('../../util');
const {insertSort} = require('../../sort');

class QuickSort {
  constructor(arr, m) {
    this.arr = shuffle(arr);
    this.m = m;
    this.depth = 0;
  }

  sort() {
    this.depth = this._sort(0, this.arr.length - 1, 0);
    insertSort(this.arr);
    assert(isSorted(this.arr));
  }

  _sort(l, r, depth) {
    // 忽略小数组
    if (r - l + 1 <= this.m) {
      return depth;
    }
    const j = this._partition(l, r);
    const leftDepth = this._sort(l, j - 1, depth + 1);
    const rightDepth = this._sort(j + 1, r, depth + 1);
    return Math.max(leftDepth, rightDepth);
  }

  _partition(l, r) {
    const v = this.arr[l];
    let i = l, j = r + 1;
    while (true) {
      while (this.arr[++i] < v) if (i === r) break;
      while (this.arr[--j] > v) if (j === l) break;
      if (i >= j) break;
      swap(this.arr, i, j);
    }
    swap(this.arr, j, l);
    return j;
  }
}

const mArr = [10, 20, 50];
const numArr = [1e3, 1e4, 1e5, 1e6];
const times = 10; // 运行10次，求平均值

const counter = {};

for (const m of mArr) {
  counter[m] = [];
  for (const num of numArr) {
    let sum = 0;
    for (let i = 0; i < times; i++) {
      const arr = genRandomDoubleArray(num);
      const sorter = new QuickSort(arr, m);
      sorter.sort();
      sum += sorter.depth;
    }
    counter[m].push(sum / times);
  }
}

const seriesData = Object.keys(counter).map(m => ({
  name: String(m),
  type: 'line',
  data: counter[m]
}));

require('fs').writeFileSync('./28.series.json', JSON.stringify(seriesData));
