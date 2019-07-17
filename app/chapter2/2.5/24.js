// 插入的时候没有问题，新加入的元素不会上浮到最上面
// 删除最大元素的时候交换下沉的时候要保证下顺序

// 官方实现：https://algs4.cs.princeton.edu/25applications/StableMinPQ.java.html

class MaxPQ {
  constructor() {
    this.data = [-1];
    this.seq = [-1]; // 元素的插入顺序
    this.sz = 0;
  }

  _swap(i, j) {
    [this.data[i], this.data[j]] = [this.data[j], this.data[i]];
    [this.seq[i], this.seq[j]] = [this.seq[j], this.seq[i]];
  }

  _less(i, j) {
    const diff = this.data[i].compareTo(this.data[j]);
    return diff === 0 ? this.seq[i] < this.seq[j] : diff < 0;
  }

  _swim(k) {
    while (k > 1) {
      const parentIndex = k >> 1;
      if (this._less(parentIndex, k)) {
        this._swap(parentIndex, k);
        k = parentIndex;
      } else {
        break;
      }
    }
  }

  insert(item) {
    const sz = ++this.sz;
    this.data[sz] = item;
    this.seq[sz] = sz;
    this._swim(sz);
  }

  _sink(k) {
    const sz = this.sz;
    while (2 * k <= sz) {
      let j = 2 * k;
      if (j < sz && this._less(j, j + 1)) {
        j++;
      }
      if (this._less(k, j)) {
        this._swap(k, j);
        k = j;
      } else {
        break;
      }
    }
  }

  delMax() {
    const item = this.data[1];
    const sz = this.sz--;
    this._swap(1, sz);
    this._sink(1);
    this.data[sz] = null;
    this.seq[sz] = 0;
    return item;
  }

  isEmpty(){
    return this.sz === 0;
  }
}

class Item {
  constructor(index, value) {
    this.index = index;
    this.value = value;
  }

  compareTo(other) {
    return this.value - other.value;
  }
}

const items = [2, 1, 2, 1, 3, 4, 3, 5, 7, 8, 5, 2, 1].map((value, index) => new Item(index, value));
const q = new MaxPQ();
for (const item of items) {
  q.insert(item);
}

while (!q.isEmpty()) {
  console.log(q.delMax());
}
