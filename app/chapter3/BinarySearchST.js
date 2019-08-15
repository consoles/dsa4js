const assert = require('assert');

// 基于平行有序数组和二分查找的符号表
class BinarySearchST {
  constructor() {
    this._keys = [];
    this._values = [];
    this.sz = 0;
  }

  /**
   * 无论你数组中是否有该键，始终返回表中小于该键的数量
   */
  _rank(key, lo, hi) {
    if (lo > hi) return lo; // 注意不是返回-1
    const mid = lo + parseInt((hi - lo) / 2);
    const midKey = this._keys[mid];
    if (midKey === key) return mid;
    return midKey > key ? this._rank(key, lo, mid - 1) : this._rank(key, mid + 1, hi);
  }

  _rank2(key, lo, hi) {
    while (lo <= hi) {
      const mid = lo + parseInt((hi - lo) / 2);
      const midKey = this._keys[mid];
      if (midKey === key) return mid;
      if (key < midKey) {
        hi = mid - 1;
      } else {
        lo = mid + 1;
      }
    }
    // 循环结束的时候lo的值正好等于表中小于被查找的键的键的数量（正确的插入位置）
    return lo;
  }

  rank(key) {
    return this._rank(key, 0, this.size - 1);
  }

  put(key, value) {
    // 查找键，找到则更新，否在在当前位置插入新的元素
    const i = this.rank(key);
    if (i >= 0 && i < this.sz && this._keys[i] === key) {
      this._values[i] = value;
      return;
    }
    for (let j = this.sz; j > i; j--) {
      this._keys[j] = this._keys[j - 1];
      this._values[j] = this._values[j - 1];
    }
    this._keys[i] = key;
    this._values[i] = value;
    this.sz++;

    // assert(this.check());
  }

  check() {
    for (let i = 0; i < this.size; i++) {
      if (i !== this.rank(this.select(i))) {
        return false;
      }
    }
    for (let i = 0; i < this.size; i++) {
      if (this._keys[i] !== this.select(this.rank(this._keys[i]))) {
        return false;
      }
    }
    return true;
  }

  get(key) {
    const i = this.rank(key);
    return i >= 0 && i < this.sz ? this._values[i] : null;
  }

  contains(key) {
    return this.get(key) !== null;
  }

  get size() {
    return this.sz;
  }

  get minKey() {
    return this._keys[0];
  }

  get maxKey() {
    return this._keys[this.sz - 1];
  }

  select(index) {
    return this._keys[index];
  }

  // 符号表中最近的一个大于等于key的元素
  ceil(key) {
    const i = this.rank(key);
    return this._keys[i];
  }

  floor(key) {
    const i = this.rank(key);
    const rankKey = this._keys[i];
    return rankKey === key ? rankKey : this._keys[i - 1];
  }

  // keys(loKey, hiKey) {
  //   const ret = [];
  //   for (let i = this.rank(loKey); i < this.rank(hiKey); i++) {
  //     ret.push(this._keys[i]);
  //   }
  //   if (this.contains(hiKey)) {
  //     ret.push(hiKey);
  //   }
  //   return ret;
  // }

  * keys() {
    for (const key of this._keys) {
      if (key) {
        yield key;
      }
    }
  }

  deleteMin() {
    this.delete(this.minKey);
  }

  deleteMax() {
    this.delete(this.maxKey);
  }

  isEmpty() {
    return this.size === 0;
  }

  delete(key) {
    // 先通过二分查找获得下标，然后后面的元素依次向前移动一位。
    const i = this.rank(key);
    if (i >= 0 && i < this.sz && this._keys[i] === key) {
      for (let j = i; j < this.sz - 1; j++) {
        this._keys[j] = this._keys[j + 1];
        this._values[j] = this._values[j + 1];
      }
      const sz = --this.sz;
      this._keys[sz] = null;
      this._values[sz] = null;
    }
    // assert(this.check());
  }
}

module.exports = BinarySearchST;

//
// const keys = 'SEARCHEXAMPLE'.split('');
// const st = new BinarySearchST();
// for (let i = 0; i < keys.length; i++) {
//   st.put(keys[i], i);
// }
// debugger;
// console.log(st.keys, st.size);
//
// st.delete('P');
// st.delete('L');
// st.delete('S');
