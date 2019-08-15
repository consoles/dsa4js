// 基于平行有序数组和二分查找的符号表
class BinarySearchST {
  constructor() {
    this._keys = [];
    this._values = [];
    this.sz = 0;
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

const {readLinesAsync} = require('../../util');

// key本来就是字典序有序的，现在对value进行排序
class FrequencyCounter{
  constructor(txtFile) {
      this.txtFile = txtFile;
  }
  async init(){
    const arr = [];
    const lines = await readLinesAsync(this.txtFile);
    for (const line of lines) {
      const words = line.split(/\s+/).map(x => x.trim()).filter(x => x.length > 0);
      for (const word of words) {
        arr.push(word);
      }
    }

    const st = new BinarySearchST();
    for (const key of arr) {
      if (st.contains(key)) {
        st.put(key,st.get(key) + 1);
      } else {
        st.put(key,1);
      }
    }

    // 单词 => 次数
    let itCount = 0;
    const MAX_IT_COUNT = 10;

    console.log('字典序排列');
    for (const key of st.keys)  {
      itCount++;
      console.log(key,'=>',st.get(key));
      if (itCount === MAX_IT_COUNT) {
        break;
      }
    }

    console.log();

    itCount = 0;
    console.log('频率降序');
    const items = [];
    for (const key of st.keys) {
      items.push([key,st.get(key)]);
    }
    items.sort((a,b) => b[1] - a[1]);
    for (const [key,value] of items) {
      itCount++;
      console.log(key,'=>',value);
      if (itCount === MAX_IT_COUNT) {
        break;
      }
    }
  }
}

const path = require('path');

(async () => {
  const counter = new FrequencyCounter(path.join(__dirname, '../../../test/input/algs4-data/tale.txt'));
  await counter.init();
})();
