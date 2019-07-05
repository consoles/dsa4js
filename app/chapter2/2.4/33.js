class IndexMinPQ {
  constructor() {
    this.sz = 0;
    this.pq = [-1]; // 索引二叉堆，从1开始
    this.qp = [-1]; // 逆序:qp[pq[i]] = pq[qp[i]] = i;
    this.values = [-1]; // 有优先级之分的元素
  }

  _less(i, j) {
    return this.values[this.pq[i]] < this.values[this.pq[j]];
  }

  _swap(i, j) {
    const tmp = this.pq[i];
    this.pq[i] = this.pq[j];
    this.pq[j] = tmp;
    this.qp[this.pq[i]] = i;
    this.qp[this.pq[j]] = j;
  }

  isEmpty() {
    return this.sz === 0;
  }

  contains(k) {
    return Number.isInteger(this.qp[k]) && this.qp[k] !== -1;
  }

  _swim(k) {
    while (k > 1) {
      const parentIndex = parseInt(k / 2);
      if (this._less(k, parentIndex)) {
        this._swap(parentIndex, k);
        k = parentIndex;
      } else {
        break;
      }
    }
  }

  insert(k, value) {
    const sz = ++this.sz;
    this.qp[k] = sz;
    this.pq[sz] = k;
    this.values[k] = value;
    this._swim(sz);
  }

  min() {
    return this.values[this.pq[1]];
  }

  _sink(k) {
    const sz = this.sz;
    while (2 * k <= sz) {
      let j = 2 * k;
      if (j < sz && this._less(j + 1, j)) {
        j++;
      }
      if (this._less(j, k)) {
        this._swap(k, j);
        k = j;
      } else {
        break;
      }
    }
  }

  delMin() {
    const index = this.pq[1];
    const sz = this.sz--;
    this._swap(1, sz);
    this._sink(1);
    const value = this.values[this.pq[sz]];
    this.values[this.pq[sz]] = null;
    this.qp[this.pq[sz + 1]] = -1;
    return {index, value};
  }

  minIndex() {
    return this.pq[1];
  }

  change(k, value) {
    this.values[k] = value;
    this._sink(this.qp[k]);
    this._swim(this.qp[k]);
  }

  delete(k) {
    const index = this.qp[k];
    this._swap(index,this.sz--);
    this._swim(index);
    this._sink(index);
    this.values[k] = null;
    this.qp[k] = -1;
  }
}

const q = new IndexMinPQ();
q.insert(2, 1);
q.insert(1, 2);
q.insert(4, 7);
q.insert(3, 6);
q.insert(5, 2);
q.insert(0, 3);
q.insert(6, 5);

q.change(1,9);

q.delete(3);

while (!q.isEmpty()) {
  console.log(q.delMin());
}
