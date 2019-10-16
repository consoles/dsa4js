const LinearProbingHashST = require('../LinearProbingHashST');

class Char {
  constructor(key, index) {
    this.key = key;
    this.index = index;
  }

  hashCode() {
    return 11 * this.index;
  }

  equals(other) {
    return this.key === other.key;
  }
}

const items = 'EASYQUTION'.split('').map((x, i) => new Char(x, i));

const st1 = new LinearProbingHashST(16);
const st2 = new LinearProbingHashST(10);
const st3 = new LinearProbingHashST(4);

for (let i = 0; i < items.length; i++) {
  st1.put(items[i], i);
  st2.put(items[i], i);
  st3.put(items[i], i);
}
debugger;

// st1和st3得到相同的结果，16是4的倍数，都会扩容成32
