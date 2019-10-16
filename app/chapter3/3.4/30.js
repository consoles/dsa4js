const _ = require('lodash');

const SeparateChainingHashST = require('../SeparateChainingHashST');

class SeparateChainingHashST30 extends SeparateChainingHashST {
  /**
   * 卡方值
   */
  get xSquqre() {
    let ret = 0;
    const d = this.size / this.M;
    for (const st of this.st) {
      ret += (st.size - d) ** 2;
    }
    return ret * this.M / this.size;
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

const N = 1000;
const M = 23;
const testCount = 10000;

const c = N / M;

const lRange = M - Math.sqrt(M);
const rRange = M + Math.sqrt(M);

console.log(lRange,rRange);

let count = 0;
for (let i = 0; i < testCount; i++) {
  const st = new SeparateChainingHashST30(M);
  for (let i = 0; i < N; i++) {
    st.put(new Int(_.random(0, 4355433)), i);
  }
  const xSquare = st.xSquqre;
  if (xSquare >= lRange && xSquare <= rRange) {
    count++;
  }
}

console.log('理论值', 1 - 1 / c, '实际值', count / testCount);

// 不知道为什么和理论值不一样？？
// 18.20416847668728 27.79583152331272
// 理论值 0.977 实际值 0
