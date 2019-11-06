class MathSet {
  constructor(keys) {
    this.set = new Set(keys);
  }

  add(key) {
    this.set.add(key);
  }

  /**
   * 补集
   * 这个还不太好实现，要枚举所有的元素，去除本身的元素
   */
  complement() {

  }

  union(key) {
    this.set.add(key);
  }

  intersection(mathSet) {
    const s = new Set();
    if (mathSet.size <= this.size) {
      for (const v of mathSet) {
        if (this.set.has(v)) {
          s.add(v);
        }
      }
    } else {
      for (const v of this.set) {
        if (mathSet.has(v)) {
          s.add(v);
        }
      }
    }
    this.set = s;
  }

  delete(key) {
    this.set.delete(key);
  }

  contains(key) {
    return this.set.has(key);
  }

  isEmpty() {
    return this.set.size === 0;
  }

  get size() {
    return this.set.size;
  }
}
