// 极端情况

// 用初始随机化和非初始随机化的快排测试练习2.1.35和练习2.1.36中描述的大型非随机数据。在将这些大数组排序时，乱序对快排的性能有何影响

const assert = require('assert');

const _ = require('lodash');

const swap = require('../../swap');
const {isSorted, shuffle} = require('../../util');
const {insertSortWithRange} = require('../../sort');

class QuickSort {
  constructor(arr, m = 20) {
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

const PD = require("probability-distributions");

const N = 1e6; // 数组规模
const times = 10; // 测试次数

function genArrHalf0Half1(len) {
  const half = len / 2;
  const arr = [];
  for (let i = 0; i < len; i++) {
    arr.push(i < half ? 0 : 1);
  }
  shuffle(arr);
  return arr;
}

function genArrHalf0Quater1Quater2(len) {
  const quater = len / 4;
  const half = len / 2;

  const arr = [];
  for (let i = 0; i < len; i++) {
    if (i < quater) {
      arr.push(1);
    } else if (i < half) {
      arr.push(2);
    } else {
      arr.push(0);
    }
  }
  shuffle(arr);
  return arr;
}

function genArrHalf0HalfRandInt(len) {
  const half = len / 2;
  const arr = [];
  for (let i = 0; i < len; i++) {
    if (i < half) {
      arr.push(0);
    } else {
      arr.push(_.random(-N, N));
    }
  }
  shuffle(arr);
  return arr;
}

const sumCounter = {
  normal: {
    'random-pivot': 0,
    'pre-shuffle': 0
  }, // 标准正态分布
  poisson: {
    'random-pivot': 0,
    'pre-shuffle': 0
  },// 泊松分布
  half0half1: {
    'random-pivot': 0,
    'pre-shuffle': 0
  }, // 一半0一半1
  half0quater1quater2: {
    'random-pivot': 0,
    'pre-shuffle': 0
  },// 一半0四分之一1四分之一2
  half0halfRandInt: {
    'random-pivot': 0,
    'pre-shuffle': 0
  } // 一半0一半随机int
};

for (let i = 0; i < times; i++) {
  let arr = PD.rnorm(N);
  let sorter = new QuickSort(arr);
  let start = Date.now();
  sorter.sortRandomPivot();
  sumCounter.normal['random-pivot'] += (Date.now() - start);
  start = Date.now();
  sorter.sortPreShuffle();
  sumCounter.normal['pre-shuffle'] += (Date.now() - start);
  console.log('done 正态分布');

  arr = PD.rpois(N, 20);
  start = Date.now();
  sorter.sortRandomPivot();
  sumCounter.poisson['random-pivot'] += (Date.now() - start);
  start = Date.now();
  sorter.sortPreShuffle();
  sumCounter.poisson['pre-shuffle'] += (Date.now() - start);
  console.log('done 泊松分布');

  arr = genArrHalf0Half1(N);
  start = Date.now();
  sorter.sortRandomPivot();
  sumCounter.half0half1['random-pivot'] += (Date.now() - start);
  start = Date.now();
  sorter.sortPreShuffle();
  sumCounter.half0half1['pre-shuffle'] += (Date.now() - start);
  console.log('done 半0半1');

  arr = genArrHalf0Quater1Quater2(N);
  start = Date.now();
  sorter.sortRandomPivot();
  sumCounter.half0quater1quater2['random-pivot'] += (Date.now() - start);
  start = Date.now();
  sorter.sortPreShuffle();
  sumCounter.half0quater1quater2['pre-shuffle'] += (Date.now() - start);
  console.log('done 一半0四分之一1四分之一2');

  arr = genArrHalf0HalfRandInt(N);
  start = Date.now();
  sorter.sortRandomPivot();
  sumCounter.half0halfRandInt['random-pivot'] += (Date.now() - start);
  start = Date.now();
  sorter.sortPreShuffle();
  sumCounter.half0halfRandInt['pre-shuffle'] += (Date.now() - start);
  console.log('done 一半0一半随机整数');
}


const sortFnSet = new Set();
const series = [];
for (const probType in sumCounter) {
  const serie = {
    name: probType,
    type: 'bar',
    barGap: 0,
    data: []
  };
  const catSum = sumCounter[probType];
  for (const sortName in catSum) {
    sortFnSet.add(sortName);
    const time = catSum[sortName] / times;
    serie.data.push(time);
  }
  series.push(serie);
}

const option = {
  tooltip: {
    trigger: 'axis',
    axisPointer: {
      type: 'shadow'
    }
  },
  legend: {
    data: Object.keys(sumCounter)
  },
  toolbox: {
    show: true,
    orient: 'vertical',
    left: 'right',
    top: 'center',
    feature: {
      mark: {show: true},
      dataView: {show: true, readOnly: false},
      magicType: {show: true, type: ['line', 'bar', 'stack', 'tiled']},
      restore: {show: true},
      saveAsImage: {show: true}
    }
  },
  xAxis: [
    {
      type: 'category',
      axisTick: {show: false},
      data: [...sortFnSet]
    }
  ],
  yAxis: [
    {
      type: 'value'
    }
  ],
  series
};

require('fs').writeFileSync('./30.echarts.option.json', JSON.stringify(option));
