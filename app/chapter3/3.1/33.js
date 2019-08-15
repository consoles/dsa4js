const _ = require('lodash');

const StopWatch = require('../StopWatch');

const {genRandomStringArr} = require('../../util');

const BinarySearchST = require('../BinarySearchST');

// 自组织查找
class ArrayST {
  constructor() {
    this.sz = 0;
    this._keys = [];
    this._values = [];
  }

  put(key, value) {
    for (let i = 0; i < this.sz; i++) {
      if (this._keys[i] === key) {
        this._values[i] = value;
        return;
      }
    }
    const sz = this.sz++;
    this._keys[sz] = key;
    this._values[sz] = value;
  }

  get(key) {
    for (let i = 0; i < this.sz; i++) {
      if (this._keys[i] === key) {
        const value = this._values[i];
        for (let j = i; j > 0; j--) {
          this._keys[j] = this._keys[j - 1];
          this._values[j] = this._values[j - 1];
        }
        this._keys[0] = key;
        this._values[0] = value;
        return value;
      }
    }
    return null;
  }

  delete(key) {
    let index = -1;
    const lastIndex = this.sz - 1;
    for (let i = 0; i <= lastIndex; i++) {
      if (this._keys[i] === key) {
        index = i;
        break;
      }
    }
    if (index !== -1) {
      this._keys[index] = this._keys[lastIndex];
      this._values[index] = this._values[lastIndex];
      this._keys[lastIndex] = null;
      this._values[lastIndex] = null;
      this.sz--;
    }
  }

  get size() {
    return this.sz;
  }
}

function stPerformanceTest(N) {

  const st1 = new BinarySearchST();
  const st2 = new ArrayST();

  const keys = genRandomStringArr(N, 5, 20);

  for (let i = 0; i < N; i++) {
    const key = keys[i];
    st1.put(key, i);
    st2.put(key, i);
  }

  // 构建查询
  keys.sort();

  let querys = [];

  let j = 0;
  const queryLen = 10 * N;

  let i = 0;
  while (i < queryLen) {
    // 1 / 2的key[0],1 / 4的keys[1],1 / 8的keys[2],1 / 16的keys[3]/3,1 / 32的keys[4]
    let len = Math.ceil(0.5 ** (j + 1) * queryLen);
    while (len-- && i < queryLen) {
      querys[i] = keys[j];
      i++;
    }
    j++;
  }
  querys = _.shuffle(querys);

  const sw = new StopWatch();

  sw.start(st1.constructor.name);
  for (let i = 0;i < querys.length;i++) {
    st1.get(querys[i]);
  }
  sw.stopAndPrint();

  sw.restart(st2.constructor.name);
  for (let i = 0;i < querys.length;i++) {
    st2.get(querys[i]);
  }
  sw.stopAndPrint();
}

for (let n = 1e3;n <= 1e6;n*=10) {
  console.log('N = ',n);
  stPerformanceTest(n);
  console.log();
}

// N =  1000
// BinarySearchST  =>  5
// ArrayST  =>  4
//
// N =  10000
// BinarySearchST  =>  39
// ArrayST  =>  3
//
// N =  100000
// BinarySearchST  =>  617
// ArrayST  =>  73
//
// N =  1000000
// BinarySearchST  =>  5257
// ArrayST  =>  567
