const SequentialSearchST = require('./SequentialSearchST');

/**
 * 基于拉链法的散列表
 */
module.exports = class SeparateChainingHashST {
  constructor(M) {

    this.initCap = 7;

    this.N = 0; // 键值对总数
    this.M = M || this.initCap; // 散列表大小（存放散列索引的数组大小）
    this.st = [];
    for (let i = 0;i < this.M;i++) {
      this.st[i] = new SequentialSearchST();// 存放链表对象的数组
    }

    // 扩容上下界
    this.UPPER_TOLERANCE = 10;
    this.LOWER_TOLERANCE = 2;
  }

  get size() {
    let count = 0;
    for (const st of this.st) {
      count += st.size;
    }
    return count;
  }

  _hash(key) {
    return (key.hashCode() & 0x7fffffff) % this.M;
  }

  * keys() {
    for (let cur of this.st) {
      while (cur) {
        yield cur.key;
        cur = cur.next;
      }
    }
  }

  get(key) {
    return this.st[this._hash(key)].get(key);
  }

  _resize(newM) {
    const newSTs = [];
    for (let i = 0;i < newM;i++) {
      newSTs[i] = new SequentialSearchST();
    }
    const oldM = this.M;
    this.M = newM;
    for (let i = 0; i < oldM; i++) {
      let cur = this.st[i].head;
      while (cur) {
        const {key, value} = cur;
        const hash = this._hash(key);
        newSTs[hash].put(key, value);
        cur = cur.next;
      }
    }
    this.st = newSTs;
  }

  put(key, value) {
    const index = this._hash(key);
    this.st[index].put(key, value);
    // 平均每个索引承载的元素哈希冲突的数量大于容忍上界就扩容
    if (this.size >= this.UPPER_TOLERANCE * this.M) {
      this._resize(2 * this.M);
    }
  }

  delete(key) {
    this.st[this._hash(key)].delete(key);
    if (this.size < this.LOWER_TOLERANCE * this.M && parseInt(this.M / 2) >= this.initCap) {
      this._resize(parseInt(this.M / 2));
    }
  }
};
