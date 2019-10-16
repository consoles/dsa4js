const _ = require('lodash');

class LinearProbingHashST39 {
  constructor(M = 16) {
    this.M = M;
    this.N = 0;
    this._keys = new Array(M);
    this._values = new Array(M);

    this.cost = 0;
  }

  _resize(cap) {
    const st = new LinearProbingHashST39(cap);
    for (let i = 0; i < this._keys.length; i++) {
      const key = this._keys[i];
      if (key) {
        st.put(key, this._values[i]);
      }
    }
    this._keys = st._keys;
    this._values = st._values;
    this.M = cap;
  }

  _hash(key) {
    return (key.hashCode() & 0x7fffffff) % this.M;
  }

  put(key, value) {
    if (this.M <= this.N * 2) {
      this._resize(2 * this.M);
    }
    let i = this._hash(key);
    for (; this._keys[i]; i = (i + 1) % this.M) {
      if (this._keys[i].equals(key)) {
        this._values[i] = value;
        return;
      }
    }
    this._keys[i] = key;
    this._values[i] = value;
    this.N++;
  }

  get(key) {
    let i = this._hash(key);
    for (; this._keys[i]; i = (i + 1) % this.M) {
      this.cost++;
      if (this._keys[i].equals(key)) {
        return this._values[i];
      }
    }
    return null;
  }
}

class Int {
  constructor(value) {
    this.value = value;
  }

  hashCode() {
    return this.value & 0x7fffffff;
  }

  equals(other) {
    return this.value === other.value;
  }
}

function test(n) {
  const st = new LinearProbingHashST39();
  let arr = [];

  for (let i = 0; i < n; i++) {
    arr.push(i);
  }
  arr = _.shuffle(arr);

  let pos = parseInt(n / 2);

  const hit = arr.slice(0, pos);
  const mis = arr.slice(pos);

  for (let i = 0; i < hit.length; i++) {
    st.put(new Int(i), i);
  }

  for (let i = 0; i < mis.length; i++) {
    // 进行n次未命中查找
    st.get(new Int(mis[i]));
  }
  return st.cost / mis.length;
}

for (const n of [1e3, 1e4, 1e5]) {
  console.log(n, test(n));
}

// 理论值未命中的查找次数为5 / 2，即2.5次，这里为啥是0.5没有搞懂

// 1000 0.508
// 10000 0.4942
// 100000 0.5013
