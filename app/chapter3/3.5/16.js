class SparseVector {
  constructor() {
    this.st = new Map();
  }

  get size() {
    return this.st.size;
  }

  put(i, x) {
    this.st.set(i, x);
  }

  get(i) {
    if (!this.st.has(i)) return 0;
    return this.st.get(i);
  }

  delete(i) {
    this.st.delete(i);
  }

  dot(that) {
    let sum = 0;
    for (let i of this.st.keys()) {
      sum += that[i] * this.get(i);
    }
    return sum;
  }

  sum(otherSparseVector) {
    for (let i of this.st.keys()) {
      const ret = this.get(i) + otherSparseVector.get(i);
      if (ret === 0) {
        this.st.delete(i);
      }
    }
  }
}
