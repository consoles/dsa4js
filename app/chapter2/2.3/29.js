// 随机化
// 用经验性的研究对比随机选择切分元素和正文所述的一开始就将数组随机化这两种策略的效果。在子数组大小为M时进行切换，将大小为N的不重复数组进行排序，其中M=10,20,50,N = 10^3~10^6

const assert = require('assert');

const _ = require('lodash');

const swap = require('../../swap');
const {isSorted, randomDoubleArray, shuffle} = require('../../util');
const {insertSortWithRange} = require('../../sort');

class QuickSort {
  constructor(arr, m = 10) {
    this.arr = arr;
    this.m = m;
  }

  // 随机选择切分元素
  sortRandomPivot() {
    this._sortRandomPivot(0, this.arr.length - 1, 0);
    assert(isSorted(this.arr), this.arr.toString());
  }

  sortPreShuffle() {
    shuffle(this.arr);
    this._sortPreShuffle(0, this.arr.length - 1);
  }

  _sortPreShuffle(l, r) {
    // 小数组插入排序
    if (l >= r) return;
    if (r - l + 1 <= this.m) {
      insertSortWithRange(this.arr, l, r);
      return;
    }
    const j = this._partitionPreShuffle(l, r);
    this._sortPreShuffle(l, j - 1);
    this._sortPreShuffle(j + 1, r);
  }

  _sortRandomPivot(l, r) {
    // 小数组插入排序
    if (l >= r) return;
    if (r - l + 1 <= this.m) {
      insertSortWithRange(this.arr, l, r);
      return;
    }
    const j = this._partitionRandomPivot(l, r);
    this._sortRandomPivot(l, j - 1);
    this._sortRandomPivot(j + 1, r);
  }

  _partitionRandomPivot(l, r) {
    const index = _.random(l, r);
    swap(this.arr, index, l);

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

  _partitionPreShuffle(l, r) {
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
  counter[m] = {
    'random-pivot': [],
    'pre-shuffle': []
  };
  for (const num of numArr) {
    let sum1 = 0;
    let sum2 = 0;
    for (let i = 0; i < times; i++) {
      const arr = randomDoubleArray(num);
      const sorter = new QuickSort(arr, m);
      let start = Date.now();
      sorter.sortRandomPivot();
      sum1 += (Date.now() - start);
      start = Date.now();
      sorter.sortPreShuffle();
      sum2 += (Date.now() - start);
    }
    counter[m]['random-pivot'].push(sum1 / times);
    counter[m]['pre-shuffle'].push(sum2 / times);
  }
}

const seriesData = [];

for (const m in counter) {
  for (const method in counter[m]) {
    seriesData.push({
      name: `${method}-${m}`,
      type: 'line',
      data: counter[m][method]
    });
  }
}

require('fs').writeFileSync('./29.series.json', JSON.stringify(seriesData));
