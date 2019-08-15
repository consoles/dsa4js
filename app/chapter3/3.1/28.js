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

  rank(key) {
    return this._rank(key, 0, this._keys.length - 1);
  }

  put(key, value) {
    // 插入一个比当前值都大的元素只需要常数时间
    // 这样在构造符号表的时候有序使用put就只需要线性时间了
    const sz = this.sz;
    if (sz === 0 || this._keys[sz - 1] <= key) {
      this._keys[sz] = key;
      this._values[sz] = value;
      this.sz++;
      return;
    }

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

  get keys(){
    return this._keys;
  }

  delete(key) {
    // 先通过二分查找获得下标，然后后面的元素依次向前移动一位。
    const i = this.rank(key);
    if (i >= 0 && j < this.sz && this._keys[i] === key) {
      for (let j = i; j < this.sz - 1; j++) {
        this._keys[j] = this._keys[j + 1];
        this._values[j] = this._values[j + 1];
      }
      this._data[--this.sz] = null;
    }
  }
}
