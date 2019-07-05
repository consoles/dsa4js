const swap = require('../../swap');

class MaxPQ {
  constructor() {
    this.data = [-1];
    this.sz = 0;
  }

  _swim(k) {
    while (k > 1) {
      const parentIndex = parseInt(k / 2);
      if (this.data[k] <= this.data[parentIndex]) {
        break;
      }
      swap(this.data, k, parentIndex);
      k = parentIndex;
    }
  }

  insert(value) {
    const sz = ++this.sz;
    this.data[sz] = value;
    this._swim(sz);
  }

  _sink(k) {
    const sz = this.sz;
    while (2 * k <= sz) {
      let j = 2 * k;
      if (j + 1 <= sz && this.data[j + 1] > this.data[j]) {
        j++;
      }
      if (this.data[k] >= this.data[j]) {
        break;
      }
      swap(this.data, k, j);
      k = j;
    }
  }

  delMax() {
    const value = this.data[1];
    this.data[this.sz] = null;
    swap(this.data, 1, this.sz--);
    this._sink(1);
    return value;
  }

  isEmpty() {
    return this.sz === 0;
  }
}

// const q = new MaxPQ();
//
// const data = [1,3,4,2,5,7,6,9];
//
// for (const item of data) {
//   q.insert(item);
//   if (q.sz > 3) {
//     q.delMax();
//   }
//
// }
//
// debugger;
//
// while (!q.isEmpty()) {
//   console.log(q.delMax());
// }

function selectFilter(points, M) {
  const q = new MaxPQ();
  const pointMap = {};
  for (let [x, y, z] of points) {
    const dis = x ** 2 + y ** 2 + z ** 2;
    q.insert(dis);
    pointMap[dis] = pointMap[dis] || [];
    if (q.sz > M) {
      q.delMax();
    }
  }
  const ret = [];
  while (!q.isEmpty()) {
    const dis = q.delMax();
    ret.push(pointMap[dis]);
  }
  return ret;
}

const M = 10 ** 4;
let n = 10 ** 5;

const _ = require('lodash');

while (true) {
  const points = [];
  for (let i = 0; i < n; i++) {
    const x = _.random(-1e5, 1e5);
    const y = _.random(-1e5, 1e5);
    const z = _.random(-1e5, 1e5);
    points.push([x, y, z]);
  }
  const start = Date.now();
  selectFilter(points, M);
  const cost = Date.now() - start;
  console.log('n = ', n, 'time = ', cost);
  n *= 2;
}

// n =  100000 time =  219
// n =  200000 time =  348
// n =  400000 time =  749
// n =  800000 time =  1505
// n =  1600000 time =  2918
// n =  3200000 time =  6060
// n =  6400000 time =  15998
// 大致是线性增长，数据规模增加一倍，运行时间增加1倍,运行时间250000ms
