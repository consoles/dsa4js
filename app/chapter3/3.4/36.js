const _ = require('lodash');

const SeparateChainingHashST = require('../SeparateChainingHashST');

class SeparateChainingHashST36 extends SeparateChainingHashST {
  get maxListLen() {
    return Math.max.apply(null, this.st.map(x => x.size));
  }

  get minListLen() {
    return Math.min.apply(null, this.st.map(x => x.size));
  }
}

class Int {
  constructor(value) {
    this.value = value;
  }

  hashCode() {
    return this.value & 0x7fffffff;
  }
}

function buildArr(n) {
  const arr = [];
  const max = parseInt(n / 100);
  for (let i = 0, id = 0; i < n; i++) {
    id++;
    if (id > max) {
      id = 1;
    }
    arr.push(id);
  }
  return _.shuffle(arr);
}

for (const n of [1e3, 1e4, 1e5]) {
  const keys = buildArr(n).map(x => new Int(x));
  const st = new SeparateChainingHashST36();
  for (let i = 0; i < keys.length; i++) {
    st.put(keys[i], i);
  }
  console.log(n, st.minListLen, st.maxListLen);
}

// 最短为0是因为hash表是根据每个链表的长度超过阈值就扩容，但是hash又比较集中（较多的hash冲突），没法利用到扩容后的空间

// 1000 0 100
// 10000 0 100
// 100000 0 100
