// 不扩容，确保输入的键值对的数量不超过M，避免出现死循环
class LinearProbingHashST {
  constructor(M) {
    this.N = 0;
    this.M = M;
    this._keys = new Array(M);
    this._values = new Array(M);

    this.compareCount = 0;
  }

  _hash(key) {
    return key.hashCode() % this.M;
  }

  put(key, value) {
    let i = this._hash(key);
    for (; this._keys[i]; i = (i + 1) % this.M) {
      this.compareCount++;
      if (key.equals(this._keys[i])) {
        this._values[i] = value;
        return;
      }
    }
    this._keys[i] = key;
    this._values[i] = value;
    this.N++;
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
}

const _ = require('lodash');

for (const M of [1e3, 1e4, 1e5]) {
  const arr = _.shuffle(_.range(M * 100)).slice(0, M);
  const st = new LinearProbingHashST(M);
  for (let i = 0; i < arr.length; i++) {
    st.put(new Int(arr[i]), i);
  }
  console.log(`M = ${M},实际值 = ${st.compareCount}，理论值 = ${Math.sqrt(Math.PI / 2) * M ** (3 / 2)}`);
}

// 为什么也不相等？
// M = 1000,实际值 = 15296，理论值 = 39633.27297606011
// M = 10000,实际值 = 619150，理论值 = 1253314.1373155
// M = 100000,实际值 = 19243499，理论值 = 39633272.97606011
