module.exports = class LinearProbingHashST {
  constructor(M = 16) {
    this.N = 0; // 符号表中键值对的总数
    this.M = M; // 线性探测表的大小
    this._keys = new Array(M); // 键
    this.values = new Array(M); // 值
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

  _hash(key) {
    return (key.hashCode() & 0x7fffffff) % this.M;
  }

  put(key, value) {
    if (this.N >= this.M / 2) this._resize(2 * this.M);
    let i = this._hash(key);
    for (; this._keys[i]; i = (i + 1) % this.M) {
      if (this._keys[i].equals(key)) {
        this.values[i] = value;
        return;
      }
    }
    this._keys[i] = key;
    this.values[i] = value;
    this.N++;
  }

  get(key) {
    for (let i = this._hash(key); this._keys[i]; i = (i + 1) % this.M) {
      if (this._keys[i].equals(key)) {
        return this.values[i];
      }
    }
    return null;
  }

  *keys(){
    for (const key of this._keys) {
      if (key) {
        yield key;
      }
    }
  }

  contains(key) {
    return this.get(key) !== null;
  }

  delete(key) {
    if (!this.contains(key)) return;
    let i = this._hash(key);
    while (!key.equals(this._keys[i])) {
      i = (i + 1) % this.M;
    }
    this._keys[i] = null;
    this.values[i] = null;
    i = (i + 1) % this.M;
    while (this._keys[i]) {
      const keyToRedo = this._keys[i];
      const valueToRedo = this.values[i];
      this._keys[i] = null;
      this.values[i] = null;
      this.N--;
      this.put(keyToRedo, valueToRedo);
      i = (i + 1) % this.M;
    }
    const N = --this.N;
    if (N > 0 && N === parseInt(N / 8)) {
      this._resize(parseInt(N / 2));
    }
  }
};
