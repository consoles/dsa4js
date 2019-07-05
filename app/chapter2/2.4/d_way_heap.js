/**
 * d叉最大堆
 */

class DWayMaxHeap {
  constructor(d) {
    this.d = d;
    this.sz = 0;
    this.data = [-1];
  }

  _sink(k) {
    const sz = this.sz;
    const d = this.d;
    while (k <= sz) {
      const start = d * (k - 1) + 2;
      // 注意边界判断
      if (start > sz) {
        break;
      }
      const end = Math.min(start + d - 1, sz);
      let j = start; // 最大值对应的索引
      for (let i = start; i <= end; i++) {
        if (this._less(j, i)) {
          j = i;
        }
      }
      if (this._less(k, j)) {
        this._swap(j, k);
        k = j;
      } else {
        break;
      }
    }
  }

  _swim(k) {
    const d = this.d;
    while (k > 1) {
      const parentIndex = Math.floor((k - 2) / d) + 1;
      if (this._less(parentIndex, k)) {
        this._swap(k, parentIndex);
        k = parentIndex;
      } else {
        break;
      }
    }
  }

  _swap(i, j) {
    [this.data[i], this.data[j]] = [this.data[j], this.data[i]];
  }

  _less(i, j) {
    return this.data[i] < this.data[j];
  }

  insert(value) {
    const sz = ++this.sz;
    this.data[sz] = value;
    this._swim(sz);
  }

  delMax() {
    const value = this.data[1];
    const sz = this.sz--;
    this._swap(1, sz);
    this._sink(1);
    return value;
  }

  isEmpty() {
    return this.sz === 0;
  }
}

const heap = new DWayMaxHeap(4);

heap.insert(1);
heap.insert(3);
heap.insert(5);
heap.insert(2);
heap.insert(9);
heap.insert(7);
heap.insert(8);
heap.insert(6);

while (!heap.isEmpty()) {
  console.log(heap.delMax());
}
