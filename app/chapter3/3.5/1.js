const SequentialSearchST = require('../SequentialSearchST');

class Set {
  constructor() {
    this.st = new SequentialSearchST();
  }

  add(key) {
    this.st.put(key, 0);
  }

  delete(key) {
    this.st.delete(key);
  }

  contains(key) {
    return this.st.contains(key);
  }

  isEmpty() {
    return this.st.size === 0;
  }

  get size() {
    return this.st.size;
  }

  toString() {
    const keys = [];
    for (const key of this.st.keys()) {
      keys.push(key);
    }
    return `{ ${keys.join(',')} },size = ${this.size}`;
  }
}

const s = new Set();

for (const item of [1,1,2,3,4,5,6]) {
  s.add(item);
}
console.log(s.toString());
s.delete(1);
console.log(s.toString());
console.log(s.contains(1), s.contains(2));
console.log(s.toString());

// hash set的实现应该是同理的,略过
