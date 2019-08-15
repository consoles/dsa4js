const _ = require('lodash');

const StopWatch = require('../StopWatch');

const path = require('path');

const {readLinesAsync} = require('../../util');

const tale = path.join(__dirname, '../../../test/input/algs4-data/tale.txt');

function genRandomBinaryNumberArray(bitsCount, N) {
  const arr = [];
  for (let i = 0; i < N; i++) {
    arr.push(_.random(2 ** bitsCount, 2 ** (bitsCount + 1) - 1));
  }
  return arr;
}

class BinarySearchST {
  constructor() {
    this._keys = [];
    this._values = [];
    this.sz = 0;

    this.watch = new StopWatch();
    this.putTime = 0;
    this.getTime = 0;
  }

  /**
   * 无论你数组中是否有该键，始终返回表中小于该键的数量
   */
  _rank(key, lo, hi) {
    if (lo > hi) return lo; // 注意不是返回-1
    const mid = lo + parseInt((hi - lo) / 2);
    const midKey = this._keys[mid];
    if (midKey === key) return mid;
    return midKey > key ? this._rank(key, lo, mid - 1) : this._rank(key, mid + 1, hi);
  }

  _rank2(key, lo, hi) {
    while (lo <= hi) {
      const mid = lo + parseInt((hi - lo) / 2);
      const midKey = this._keys[mid];
      if (midKey === key) return mid;
      if (key < midKey) {
        hi = mid - 1;
      } else {
        lo = mid + 1;
      }
    }
    // 循环结束的时候lo的值正好等于表中小于被查找的键的键的数量（正确的插入位置）
    return lo;
  }

  rank(key) {
    return this._rank(key, 0, this.keys.length - 1);
  }

  put(key, value) {
    // 查找键，找到则更新，否在在当前位置插入新的元素
    this.watch.start();
    const i = this.rank(key);
    if (i >= 0 && i < this.sz && this._keys[i] === key) {
      this._values[i] = value;
      return;
    }
    for (let j = this.sz; j > i; j--) {
      this._keys[j] = this._keys[j - 1];
      this._values[j] = this._values[j - 1];
    }
    this._keys[i] = key;
    this._values[i] = value;
    this.sz++;
    this.putTime += this.watch.elapseTime;
    // assert(this.check());
  }

  check() {
    for (let i = 0; i < this.size; i++) {
      if (i !== this.rank(this.select(i))) {
        return false;
      }
    }
    for (let i = 0; i < this.size; i++) {
      if (this._keys[i] !== this.select(this.rank(this._keys[i]))) {
        return false;
      }
    }
    return true;
  }

  get(key) {
    this.watch.start();
    const i = this.rank(key);
    const ret = i >= 0 && i < this.sz ? this._values[i] : null;
    this.getTime += this.watch.elapseTime;
    return ret;
  }

  contains(key) {
    return this.get(key) !== null;
  }

  get size() {
    return this.sz;
  }

  get minKey() {
    return this._keys[0];
  }

  get maxKey() {
    return this._keys[this.sz - 1];
  }

  select(index) {
    return this._keys[index];
  }

  // 符号表中最近的一个大于等于key的元素
  ceil(key) {
    const i = this.rank(key);
    return this._keys[i];
  }

  floor(key) {
    const i = this.rank(key);
    const rankKey = this._keys[i];
    return rankKey === key ? rankKey : this._keys[i - 1];
  }

  // keys(loKey, hiKey) {
  //   const ret = [];
  //   for (let i = this.rank(loKey); i < this.rank(hiKey); i++) {
  //     ret.push(this._keys[i]);
  //   }
  //   if (this.contains(hiKey)) {
  //     ret.push(hiKey);
  //   }
  //   return ret;
  // }

  get keys() {
    return this._keys.filter(x => !!x);
  }

  deleteMin() {
    this.delete(this.minKey);
  }

  deleteMax() {
    this.delete(this.maxKey);
  }

  isEmpty() {
    return this.size === 0;
  }

  delete(key) {
    // 先通过二分查找获得下标，然后后面的元素依次向前移动一位。
    const i = this.rank(key);
    if (i >= 0 && i < this.sz && this._keys[i] === key) {
      for (let j = i; j < this.sz - 1; j++) {
        this._keys[j] = this._keys[j + 1];
        this._values[j] = this._values[j + 1];
      }
      const sz = --this.sz;
      this._keys[sz] = null;
      this._values[sz] = null;
    }
    // assert(this.check());
  }
}

function wordCount(ST, arr) {
  let totalTime = 0;
  let putTime = 0;
  let getTime = 0;
  let testCount = 10;

  for (let i = 0; i < testCount; i++) {
    const start = Date.now();
    const st = new ST();
    for (const key of arr) {
      if (st.contains(key)) {
        st.put(key, st.get(key) + 1);
      } else {
        st.put(key, 1);
      }
    }
    let maxKey = -1;
    st.put(maxKey, 0);
    for (const key of st.keys) {
      if (st.get(key) > st.get(maxKey)) {
        maxKey = key;
      }
    }
    totalTime += (Date.now() - start);
    putTime += st.putTime;
    getTime += st.getTime;
  }

  return {
    total: totalTime / testCount,
    put: putTime / testCount,
    get: getTime / testCount,
  };
}

const N = 1e6;

const Ms = [10, 20, 30];

// for (const M of Ms) {
//   const arr = genRandomBinaryNumberArray(M, N);
//   const ret = wordCount(BinarySearchST, arr);
//   console.log('M = ', M, JSON.stringify(ret));
// }

console.log('统计tale.txt');

(async () => {
  const words = []; // len 135643
  const lines = await readLinesAsync(tale);

  for (const line of lines) {
    const wordsInLine = line.split(/\s+/).map(x => x.trim()).filter(x => x.length > 0);
    for (const word of wordsInLine) {
      words.push(word);
    }
  }

  const ret = wordCount(BinarySearchST, words);
  console.log('tale.txt', JSON.stringify(ret));
})();

// M =  10 {"total":453456.2,"put":83.4,"get":294018.8}
// 统计tale.txt
// tale.txt {"total":20618.9,"put":483.8,"get":14219.6}

// get的耗时要远远超过put
