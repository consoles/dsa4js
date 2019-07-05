// 动态中位数查找
const swap = require('../../swap');

class MaxPQ {
  constructor() {
    this.data = [-1];
    this.sz = 0;
  }

  less(i, j) {
    return this.data[i] < this.data[j];
  }

  _swim(k) {
    while (k > 1) {
      const parentIndex = parseInt(k / 2);
      if (this.less(parentIndex, k)) {
        swap(this.data, k, parentIndex);
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
        swap(this.data, j, k);
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
    swap(this.data, 1, sz);
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

class MinPQ extends MaxPQ {
  less(i, j) {
    return this.data[j] < this.data[i];
  }

  min() {
    return super.max();
  }

  delMin() {
    return super.delMax();
  }

  max() {
    throw new Error('没有这个方法');
  }

  delMax() {
    throw new Error('没有这个方法');
  }
}

// 面向中位数的堆
class MedianPQ {
  constructor() {
    this.maxPQ = new MaxPQ(); // 最大堆，保存前半段元素
    this.minPQ = new MinPQ(); // 最小堆，保存后半段元素
    this.median = null; // 中位数
    this.sz = 0; // 堆大小
  }

  insert(value) {
    if (this.sz++ === 0) {
      this.median = value;
      return;
    }
    if (value < this.median) {
      this.maxPQ.insert(value);
    } else {
      this.minPQ.insert(value);
    }
    this.updateMedian();
  }

  /**
   * 删除并返回中位数
   */
  delMedian() {
    const median = this.median;
    if (--this.sz === 0) {
      this.median = null;
      return median;
    }
    // 从较大的一侧堆中取出元素作为新的中位数
    if (this.minPQ.sz > this.maxPQ.sz) {
      this.median = this.minPQ.delMin();
    } else {
      this.median = this.maxPQ.delMax();
    }
    return median;
  }

  isEmpty() {
    return this.sz === 0;
  }

  /**
   * 根据两个堆的大小调整中位数
   */
  updateMedian() {
    while (this.maxPQ.sz - this.minPQ.sz > 1) {
      this.minPQ.insert(this.median);
      this.median = this.maxPQ.delMax();
    }
    while (this.minPQ.sz - this.maxPQ.sz > 1) {
      this.maxPQ.insert(this.median);
      this.median = this.minPQ.delMin();
    }
  }
}

const q = new MedianPQ();
q.insert(1);
q.insert(3);
q.insert(4);
q.insert(2);
q.insert(7);
q.insert(5);
q.insert(8);

while (!q.isEmpty()) {
  console.log(q.delMedian());
}
