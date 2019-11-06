// 用数组实现，最简单

class MultiSet {
  constructor() {
    this.arr = [];
  }

  add(key) {
    this.arr.push(key);
  }

  delete(key) {
    this.arr = this.arr.filter(x => x !== key);
  }

  contains(key) {
    return this.arr.indexOf(key) !== -1;
  }

  isEmpty() {
    return this.arr.length === 0;
  }

  get size() {
    return this.arr.length;
  }

  toString() {
    const keys = [];
    for (const key of this.arr) {
      keys.push(key);
    }
    return `{ ${keys.join(',')} },size = ${this.size}`;
  }
}
