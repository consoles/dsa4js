class LinearProbingHashST {
  constructor(M = 16) {
    this.N = 0; // 符号表中键值对的总数
    this.M = M; // 线性探测表的大小
    this._keys = new Array(M); // 键
    this.values = new Array(M); // 值
  }

  get size() {
    return this.N;
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

  * keys() {
    for (const key of this._keys) {
      if (key) {
        yield key;
      }
    }
  }

  contains(key) {
    return this.get(key) !== null;
  }

  toString() {
    const items = [];
    for (const key of this.keys()) {
      items.push(key);
    }
    return `size = ${this.size} , { ${items.join(',')} } `;
  }

  delete(key) {
    if (!this.contains(key)) return;
    let i = this._hash(key);

    while (this._keys[i].equals(key)) {
      this._keys[i] = null;
      this.values[i] = null;
      this.N--;
      i = (i + 1) % this.M;
    }

    while (this._keys[i]) {
      const keyToRedo = this._keys[i];
      const valueToRedo = this.values[i];
      this._keys[i] = null;
      this.values[i] = null;
      // 这里减1的原因是put会导致自增
      this.N--;
      if (keyToRedo !== key) {
        this.put(keyToRedo, valueToRedo);
      }
      i = (i + 1) % this.M;
    }
    if (this.N > 0 && this.N === parseInt(this.N / 8)) {
      this._resize(parseInt(this.N / 2));
    }
  }
}

class Int {
  constructor(value) {
    this.value = value;
  }

  hashCode() {
    return this.value;
  }

  equals(other) {
    return this.value === other.value;
  }

  toString() {
    return this.value;
  }
}

const s = new LinearProbingHashST();

for (const item of [1, 1, 2, 3, 4, 5, 6]) {
  s.put(new Int(item), item);
}
console.log(s.toString());
console.log(s.get(new Int(1)));
s.delete(new Int(1));
console.log(s.get(new Int(1)));
console.log(s.toString());
console.log(s.contains(new Int(1)), s.contains(new Int(2)));
console.log(s.toString());
