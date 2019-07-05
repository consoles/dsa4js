class MaxPQ {
  constructor() {
    this.data = [-1];
    this.sz = 0;
  }

  isEmpty() {
    return this.sz === 0;
  }

  _sink(k) {
    const key = this.data[k];
    while (2 * k <= this.sz) {
      let j = 2 * k;
      if (j + 1 <= this.sz && this.data[j + 1] > this.data[j]) {
        j++;
      }
      if (this.data[j] <= key) {
        break;
      }
      this.data[k] = this.data[j];
      k = j;
    }
    this.data[k] = key;
  }

  insert(value) {
    const sz = ++this.sz;
    this.data[sz] = value;
    this._swim(sz);
  }

  _swim(k) {
    const key = this.data[k];
    while (k > 1 && this.data[parseInt(k / 2)] < key) {
      this.data[k] = this.data[parseInt(k / 2)];
      k = parseInt(k / 2);
    }
    this.data[k] = key;
  }

  delMax() {
    const value = this.data[1];
    const sz = this.sz--;
    [this.data[1], this.data[sz]] = [this.data[sz], this.data[1]];
    this._sink(1);
    return value;
  }
}

const q = new MaxPQ();
q.insert(2);
q.insert(3);
q.insert(1);
q.insert(5);
q.insert(4);
while (!q.isEmpty()) {
  console.log(q.delMax());
}
