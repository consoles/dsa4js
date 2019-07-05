// 同时面向最大和最小元素的优先队列。设计一个数据结构在对数时间插入元素、删除最大元素；常数级别找到最大元素和最小元素
// 提示：用2个堆

class MinMaxNode {
  constructor(value, index) {
    this.value = value;
    this.index = index;
    this.pair = null;
  }

  /**
   * 工厂方法，建立两个孪生节点
   * @param value
   * @param index
   * @returns {{minNode: MinMaxNode, maxNode: MinMaxNode}}
   */
  static getNodes(value, index) {
    const minNode = new MinMaxNode(value, index);
    const maxNode = new MinMaxNode(value, index);
    minNode.pair = maxNode;
    maxNode.pair = minNode;
    return {minNode, maxNode};
  }
}

class MaxPQ {
  constructor() {
    this.data = [-1];
    this.sz = 0;
  }

  less(i, j) {
    return this.data[i].value < this.data[j].value;
  }

  swap(i, j) {
    this.data[i].pair.pair = this.data[j];
    this.data[j].pair.pair = this.data[i];

    const swapNode = this.data[i].pair;
    const swapValue = this.data[i].value;

    this.data[i].value = this.data[j].value;
    this.data[i].pair = this.data[j].pair;

    this.data[j].value = swapValue;
    this.data[j].pair = swapNode;
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

  /**
   * 删除指定索引的元素
   */
  remove(k) {
    const sz = this.sz--;
    if (k === sz) {
      this.data[sz] = null;
      return;
    }
    if (this.sz <= 2) {
      this.swap(1, k);
      this.data[sz] = null;
      return;
    }
    this.swap(k, sz);
    this.data[sz] = null;
    this._swim(k);
    this._sink(k);
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
    return this.data[j].value < this.data[i].value;
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

class MinMaxPQ {
  constructor() {
    this.minPQ = new MinPQ();
    this.maxPQ = new MaxPQ();
    this.sz = 0;
  }

  delMax() {
    this.minPQ.remove(this.maxPQ.max().pair.index);
    const value = this.maxPQ.max().value;
    this.maxPQ.delMax();
    this.sz--;
    return value;
  }

  delMin() {
    this.maxPQ.remove(this.minPQ.min().pair.index);
    const value = this.minPQ.min().value;
    this.minPQ.delMin();
    this.sz--;
    return value;
  }

  insert(value) {
    const {minNode, maxNode} = MinMaxNode.getNodes(value, ++this.sz);
    this.maxPQ.insert(maxNode);
    this.minPQ.insert(minNode);
  }

  isEmpty() {
    return this.sz === 0;
  }

  max() {
    return this.maxPQ.max().value;
  }

  min() {
    return this.minPQ.min().value;
  }
}

const q = new MinMaxPQ();
q.insert(1);
q.insert(5);
q.insert(9);
q.insert(2);
q.insert(4);
q.insert(7);
q.insert(3);

let value = q.delMax();
value = q.delMin();
value = q.delMin();
value = q.delMax();
value = q.delMin();

while (!q.isEmpty()) {
  console.log(q.delMax());
}
debugger;
