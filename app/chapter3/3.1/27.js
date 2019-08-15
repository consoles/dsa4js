// 事实上就是说，先构造一个包含 N 个不重复键的符号表，然后进行 S 次查找。
// 给出 S 的增长数量级，使得构造符号表的成本和查找的成本相同。
//
// 这里假设一次数组交换和一次比较的成本是相同的。
// 先考虑构造符号表的成本，一次 Put() 需要调用一次 Rank() 和一次插入操作。
// 2.1 节插入排序的命题 B 给出了每次插入平均需要移动一半的数组元素的结论。
// 于是构造符号表所需的成本约为：NlgN + (N - 1) * N / 2 / 2。
// 这里查找的成本是这么计算的：lg0+lg1+⋯+lgN <NlgN
// 查找所需的成本比较简单，一次二分查找的比较次数约为 lgN，总成本就是 S*lgN 。
// 令两边相等，解方程即可得到 S = N + N*(N−1)/4lgN 。
//
// 如果用大 O 记法，也可以记为 O(n^2/lgn)，如果要选择一个比较常用的上界则可以选择 O(n^2)。

const count = 10000;

const keys = [];
for (let i = 0; i < count; i++) {
  keys.push(Math.random());
}

const _ = require('lodash');

class BinarySearchST {
  constructor() {
    this._keys = [];
    this._values = [];
    this.sz = 0;

    this.compareCount = 0;
    this.swapCount = 0;
  }

  /**
   * 无论你数组中是否有该键，始终返回表中小于该键的数量
   */
  _rank(key, lo, hi) {
    if (lo > hi) return lo; // 注意不是返回-1
    const mid = lo + parseInt((hi - lo) / 2);
    const midKey = this._keys[mid];
    this.compareCount++;
    if (midKey === key) return mid;
    return midKey > key ? this._rank(key, lo, mid - 1) : this._rank(key, mid + 1, hi);
  }

  rank(key) {
    return this._rank(key, 0, this._keys.length - 1);
  }

  put(key, value) {
    // 查找键，找到则更新，否在在当前位置插入新的元素
    const i = this.rank(key);
    if (i >= 0 && i < this.sz && this._keys[i] === key) {
      this._values[i] = value;
      return;
    }
    for (let j = this.sz; j > i; j--) {
      this.swapCount++;
      this._keys[j] = this._keys[j - 1];
      this._values[j] = this._values[j - 1];
    }
    this._keys[i] = key;
    this._values[i] = value;
    this.sz++;
  }

  get(key) {
    const i = this.rank(key);
    return i >= 0 && i < this.sz ? this._values[i] : null;
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

  get keys(){
    return this._keys;
  }

  delete(key) {
    // 先通过二分查找获得下标，然后后面的元素依次向前移动一位。
    const i = this.rank(key);
    if (i >= 0 && j < this.sz && this._keys[i] === key) {
      for (let j = i; j < this.sz - 1; j++) {
        this._keys[j] = this._keys[j + 1];
        this._values[j] = this._values[j + 1];
      }
      this._data[--this.sz] = null;
    }
  }
}

const st = new BinarySearchST();

for (const key of keys) {
  st.put(key, 1);
}

console.log('put操作','比较次数',st.compareCount,'交换次数',st.swapCount,'总成本',st.compareCount + st.swapCount);

let c = count + Math.floor( count * (count - 1) / (4 * Math.log2(count)));
st.compareCount = 0;
st.swapCount = 0;
while (c--) {
  const key = _.sample(keys);
  st.get(key);
}
console.log('N + N*(N−1)/4lgN','比较次数',st.compareCount,'交换次数',st.swapCount,'总成本',st.compareCount + st.swapCount);

c = Math.floor(count ** 2 / Math.log2(count));
st.compareCount = 0;
st.swapCount = 0;
while (c--) {
  const key = _.sample(keys);
  st.get(key);
}
console.log('N^2/lgN','比较次数',st.compareCount,'交换次数',st.swapCount,'总成本',st.compareCount + st.swapCount);

c = count ** 2;
st.compareCount = 0;
st.swapCount = 0;
while (c--) {
  const key = _.sample(keys);
  st.get(key);
}
console.log('N^2','比较次数',st.compareCount,'交换次数',st.swapCount,'总成本',st.compareCount + st.swapCount);

// put操作 比较次数 119065 交换次数 25071362 总成本 25190427
// N + N*(N−1)/4lgN 比较次数 23378583 交换次数 0 总成本 23378583
// N^2/lgN 比较次数 93036748 交换次数 0 总成本 93036748
// N^2 比较次数 1236328255 交换次数 0 总成本 1236328255
