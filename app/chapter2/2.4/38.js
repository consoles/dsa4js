const ns = [1e5, 2e5, 4e5, 8e5];
const testCount = 10;

class MaxPQ {
  constructor() {
    this.data = [-1];
    this.sz = 0;
  }

  less(i, j) {
    return this.data[i] < this.data[j];
  }

  swap(i, j) {
    [this.data[i], this.data[j]] = [this.data[j], this.data[i]];
  }

  _swim(k) {
    while (k > 1) {
      const parentIndex = parseInt(k / 2);
      if (this.less(parentIndex, k)) {
        this.swap(k, parentIndex);
        k = parentIndex;
      } else {
        break;
      }
    }
  }

  _sink(k) {
    const sz = this.sz;
    while (2 * k <= sz) {
      let j = 2 * k;
      if (j < sz && this.less(j, j + 1)) {
        j++;
      }
      if (this.less(j, k)) {
        break;
      } else {
        this.swap(j, k);
        k = j;
      }
    }
  }

  insert(value) {
    const sz = ++this.sz;
    this.data[sz] = value;
    this._swim(sz);
  }

  delMax() {
    const value = this.data[1];
    const sz = this.sz--;
    this.swap(1, sz);
    this.data[sz] = null;
    this._sink(1);
    return value;
  }

  max() {
    return this.data[1];
  }

  isEmpty() {
    return this.sz === 0;
  }
}

const utils = require('../../util');

const {
  genTwoElementsArr,
  genArrSame,
  genArrDesc,
  genArrAsc
} = utils;

const genMethods = [genTwoElementsArr,
  genArrSame,
  genArrDesc,
  genArrAsc];

for (let n of ns) {
  for (method of genMethods) {
    let total = 0;
    for (let i = 0; i < testCount; i++) {
      const arr = method.call(utils, n);
      const start = Date.now();
      const pq = new MaxPQ();
      for (const item of arr) {
        pq.insert(item);
      }
      while (!pq.isEmpty()) {
        pq.delMax();
      }
      const elapse = Date.now() - start;
      total += elapse;
    }
    console.log('n = ', n, '数组类型', method.name, 'took', total / testCount);
  }
}

// n =  100000 数组类型 genTwoElementsArr took 120.9
// n =  100000 数组类型 genArrSame took 95.9
// n =  100000 数组类型 genArrDesc took 103.8
// n =  100000 数组类型 genArrAsc took 174.9
// n =  200000 数组类型 genTwoElementsArr took 230.7
// n =  200000 数组类型 genArrSame took 210.9
// n =  200000 数组类型 genArrDesc took 240.7
// n =  200000 数组类型 genArrAsc took 389.6
// n =  400000 数组类型 genTwoElementsArr took 505.4
// n =  400000 数组类型 genArrSame took 454.5
// n =  400000 数组类型 genArrDesc took 580.7
// n =  400000 数组类型 genArrAsc took 840.7
// n =  800000 数组类型 genTwoElementsArr took 1053
// n =  800000 数组类型 genArrSame took 983.1
// n =  800000 数组类型 genArrDesc took 1149.4
// n =  800000 数组类型 genArrAsc took 1784.9

// 最大堆来说顺序时会比较慢，因为每次插入都要一路上升到顶部。
// 逆序的时候则是删除比较慢，最后一个元素是最小的元素，交换后需要一路下沉到底部。
// 由于元素相同的时候我们选择不交换（less(i, j) 返回 false），较多的重复元素并不会影响性能。
