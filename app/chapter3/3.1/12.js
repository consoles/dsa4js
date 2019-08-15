const _ = require('lodash');

class Item {
  constructor(key, value) {
    this.key = key;
    this.value = value;
  }

  compareTo(other) {
    return this.key.localeCompare(other.key);
  }
}

function mergeSort(items) {
  function _mergeSort(lo, hi) {
    if (lo >= hi) return;
    const mid = lo + parseInt((hi - lo) / 2);
    _mergeSort(lo, mid);
    _mergeSort(mid + 1, hi);
    _merge(lo, mid, hi);
  }

  function _merge(lo, mid, hi) {
    const aux = [];
    let i = lo, j = mid + 1;
    for (let k = lo; k <= hi; k++) {
      if (j > hi) {
        aux.push(items[i++]);
      } else if (i > mid) {
        aux.push(items[j++]);
      } else if (items[i].compareTo(items[j]) < 0) {
        aux.push(items[i++]);
      } else {
        aux.push(items[j++]);
      }
    }
    for (let k = 0; k < aux.length; k++) {
      items[lo + k] = aux[k];
    }
  }

  _mergeSort(0, items.length - 1);
}

class BinarySearchST {
  constructor(items = []) {
    mergeSort(items);
    this._data = items;
    this.sz = items.length;
  }

  /**
   * 无论你数组中是否有该键，始终返回表中小于该键的数量
   */
  _rank(key, lo, hi) {
    if (lo > hi) return lo; // 注意不是返回-1
    const mid = lo + parseInt((hi - lo) / 2);
    const midKey = this._data[mid].key;
    if (midKey === key) return mid;
    return midKey > key ? this._rank(key, lo, mid - 1) : this._rank(key, mid + 1, hi);
  }

  rank(key) {
    return this._rank(key, 0, this._data.length - 1);
  }

  put(key, value) {
    // 查找键，找到则更新，否在在当前位置插入新的元素
    const i = this.rank(key);
    if (i >= 0 && i < this.sz && this._data[i].key === key) {
      this._data[i].value = value;
      return;
    }
    for (let j = this.sz; j > i; j--) {
      this._data[j] = this._data[j - 1];
    }
    this._data[i] = {key, value};
    this.sz++;
  }

  get(key) {
    const i = this.rank(key);
    return i >= 0 && i < this.sz ? this._data[i].value : null;
  }

  contains(key) {
    return this.get(key) !== null;
  }

  get size() {
    return this.sz;
  }

  get minKey() {
    return this._data[0].key;
  }

  get maxKey() {
    return this._data[this.sz - 1].key;
  }

  select(index) {
    return this._data[index].key;
  }

  // 符号表中最近的一个大于等于key的元素
  ceil(key) {
    const i = this.rank(key);
    return this._data[i].key;
  }

  floor(key) {
    const i = this.rank(key);
    const rankKey = this._data[i].key;
    return rankKey === key ? rankKey : this._data[i - 1].key;
  }

  keys(loKey, hiKey) {
    const ret = [];
    for (let i = this.rank(loKey); i < this.rank(hiKey); i++) {
      ret.push(this._data[i].key);
    }
    if (this.contains(hiKey)) {
      ret.push(hiKey);
    }
    return ret;
  }

  delete(key) {
    const i = this.rank(key);
    if (i >= 0 && i < this.sz && this._data[i].key === key) {
      for (let j = i; j < this.sz - 1; j++) {
        this._data[j] = this._data[j + 1];
      }
    }
    this._data[--this.sz] = null;
  }
}

// 注意：自己对key进行去重处理
const keys = _.uniq('SEARCHEXAMPLE'.split('')).map((value,index) => new Item(value,index));
const st = new BinarySearchST(keys);

debugger;
console.log(st.keys, st.size);

st.delete('A');
st.delete('L');
st.delete('X');
