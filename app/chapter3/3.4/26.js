'use strict';

/**
 * 线性探测法中的延迟删除
 *
 * 在删除一个键值对的时候将其值设置为null，并在调用resize方法的时候将键值对从表中删除。主要难点在于何时调用resize
 *
 * 注意：如果后来的put方法为该键指定了一个新的值，应该用新值将null覆盖掉。程序在决定扩容或者缩容的时候不但要考虑数组中的空元素，也要考虑这种死掉的元素
 */

class LinearProbingHashST {

  constructor(M = 16) {
    this.M = M;
    this.N = 0;
    this._keys = new Array(M);
    this._values = new Array(M);
  }

  _hash(key) {
    return (key.hashCode() & 0x7fffffff) % this.M;
  }

  put(key, value) {
    if (this.N >= this.M / 2) this._resize(2 * this.M);
    let hash = i = this._hash(key);

    while (this._keys[i]) {
      if (this._keys[i].equals(key)) {
        return this._values[i] = value;
      }
      i = (i + 1) % this.M;
      if (i === hash) {
        throw new Error('hash table is full');
      }
    }

    this._keys[i] = key;
    this._values[i] = value;
    this.N++;
  }

  get(key) {
    const start = this._hash(key);
    let i = start;
    do {
      if (this._keys[i] && this._keys[i].equals(key)) {
        return this._values[i];
      }
      i = (i + 1) % this.M;
    } while (i !== start);
    return null;
  }

  _resize(cap) {
    const t = new LinearProbingHashST(cap);
    for (let i = 0; i < this.M; i++) {
      if (this._keys[i]) {
        t.put(this._keys[i], this.values[i]);
      }
    }
    this._keys = t._keys;
    this.values = t.values;
    this.M = t.M;
  }

  contains(key) {
    return this.get(key) !== null;
  }

  delete(key) {
    if (!this.contains(key)) return;
    let i = hash = this._hash(key);
    while (!key.equals(this._keys[i])) {
      i = (i + 1) % this.M;
      if (i === hash) {
        throw new Error('hash table is full');
      }
    }
    this._keys[i] = null;
    this.values[i] = null;
    const N = --this.N;
    if (N > 0 && N === parseInt(N / 8)) {
      this._resize(parseInt(N / 2));
    }
  }
}
