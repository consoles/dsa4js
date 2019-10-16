const _ = require('lodash');
const {randomArray, genRandomStringArr} = require('../../util');

class Int {
  constructor(value) {
    this.value = value;
  }

  hash() {
    return this.value & 0x7fffffff;
  }

  compareTo(other) {
    return this.value === other.value;
  }
}

class Str {
  constructor(value) {
    this.value = value;
  }

  hash() {
    let hash = 0;
    for (let i = 0; i < this.value.length; i++) {
      hash = hash * 31 + this.value.charCodeAt(i);
    }
    return hash & 0x7fffffff;
  }

  compareTo(other) {
    return this.value === other.value;
  }
}

const testCount = 1e2;

const counter = {
  int: {
    hash: 0,
    compareTo: 0
  },
  str: {
    hash: 0,
    compareTo: 0
  }
};

for (let i = 0; i < testCount; i++) {
  let arr = randomArray(0, 1e5).map(x => new Int(x));
  let start = Date.now();
  for (const item of arr) {
    item.hash();
  }
  let end = Date.now();
  counter.int.hash = end - start;
  start = Date.now();
  for (const item of arr) {
    item.compareTo(_.sample(arr));
  }
  end = Date.now();
  counter.int.compareTo = end - start;

  arr = genRandomStringArr(1e5, 5, 40).map(x => new Str(x));
  start = Date.now();
  for (const item of arr) {
    item.hash();
  }
  end = Date.now();
  counter.str.hash = end - start;
  start = Date.now();
  for (const item of arr) {
    item.compareTo(_.sample(arr));
  }
  end = Date.now();
  counter.str.compareTo = end - start;
}

console.log(counter);
