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
    return this._rank(key, 0, this._keys.length - 1);
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

  keys(loKey, hiKey) {
    const ret = [];
    for (let i = this.rank(loKey); i < this.rank(hiKey); i++) {
      ret.push(this._keys[i]);
    }
    if (this.contains(hiKey)) {
      ret.push(hiKey);
    }
    return ret;
  }

  get allKeys() {
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

/**
 * 符号表，基于有序表，利用插值法实现搜索。
 *
 * 这种实现要求key支持算术运算（通过加减得到偏移量）
 */
class InterpolationSearchST extends BinarySearchST {
  rank(key) {
    let lo = 0, hi = this.sz - 1;
    while (lo <= hi) {
      let index = -1;
      if (lo === hi) {
        index = lo;
      } else {
        const percent = (key - this._keys[lo]) / (this._keys[hi] - this._keys[lo]);
        if (percent < 0) {
          index = lo;
        } else if (percent > 1) {
          index = hi;
        } else {
          index = lo + Math.floor((hi - lo) * percent);
        }
      }
      const compare = this._keys[index] - key;
      if (compare > 0) {
        hi = index - 1;
      } else if (compare < 0) {
        lo = index + 1;
      } else {
        return index;
      }
    }
    return lo;
  }
}

const {randomArray} = require('../../util');

const testCount = 10;

function wordCount(st, arr) {
  for (const key of arr) {
    if (st.contains(key)) {
      st.put(key, st.get(key) + 1);
    } else {
      st.put(key, 1);
    }
  }
  let maxKey = -1;
  st.put(maxKey, 0);
  for (const key of st.allKeys) {
    if (st.get(key) > st.get(maxKey)) {
      maxKey = key;
    }
  }
  return maxKey;
}

for (let n = 1e3; n <= 1e6; n *= 10) {

  let t1 = t2 = 0;

  for (let i = 0; i < testCount; i++) {
    const arr = randomArray(0, parseInt(n / 2), n);
    let start = Date.now();
    let st = new BinarySearchST();
    wordCount(st, arr);
    t1 += (Date.now() - start);
    start = Date.now();
    st = new InterpolationSearchST();
    wordCount(st, arr);
    t2 += (Date.now() - start);
  }

  console.log('n = ', n, '二分查找表', t1 / testCount, '插值查找表', t2 / testCount);
}

// 这种方法对于分布相对均匀的数组比较有利，相对于二分查找而言迭代次数会少很多。但如果数组分布不够均匀，也可能表现出不如二分查找的性能。

// n =  1000 二分查找表 5 插值查找表 2.9
// n =  10000 二分查找表 75.7 插值查找表 59.4
// n =  100000 二分查找表 5094.6 插值查找表 4788.1
