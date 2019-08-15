// 维持一个变量cost，每次比较和交换都令cost加1

class Node {
  constructor(key, value, next) {
    this.key = key;
    this.value = value;
    this.next = next || null;
  }
}

class SequentialSearchST {

  constructor() {
    this.head = null;
    this.sz = 0;
    this.cost = 0;
  }

  put(key, value) {
    if (!this.head) {
      this.head = new Node(key, value);
      this.sz++;
    } else {
      // 向后扫描，如果找到相同的元素就将key替换为value
      let cur = this.head;
      while (cur) {
        this.cost++;
        if (cur.key === key) {
          cur.value = value;
          return;
        }
        cur = cur.next;
      }
      // 遍历完链表，没有找到，将新节点插入头部
      this.head = new Node(key, value, this.head);
      this.sz++;
    }
  }

  get(key) {
    if (!this.head) return null;
    let cur = this.head;
    while (cur) {
      if (cur.key === key) {
        return cur.value;
      }
      cur = cur.next;
    }
    return null;
  }

  contains(key) {
    return this.get(key) !== null;
  }

  get size() {
    return this.sz;
  }

  get keys() {
    const ret = [];
    if (!this.head) return ret;
    for (let cur = this.head; cur; cur = cur.next) {
      ret.push(cur.key);
    }
    return ret;
  }

  delete(key) {
    // 找到待删除的节点的父节点
    const dummyHead = new Node(null, null, this.head);
    let parent = dummyHead;
    while (parent) {
      if (parent.next && parent.next.key === key) {
        break;
      }
      parent = parent.next;
    }
    if (!parent) {
      throw new Error(`no such key ${key}`);
    }
    parent.next = parent.next.next;
    this.head = dummyHead.next;
    dummyHead.next = null;
    this.sz--;
  }

}

class BinarySearchST {
  constructor() {
    this._keys = [];
    this._values = [];
    this.sz = 0;
    this.cost = 0;
  }

  /**
   * 无论你数组中是否有该键，始终返回表中小于该键的数量
   */
  _rank(key, lo, hi) {
    if (lo > hi) return lo; // 注意不是返回-1
    const mid = lo + parseInt((hi - lo) / 2);
    const midKey = this._keys[mid];
    this.cost++;
    if (midKey === key) return mid;
    this.cost++;
    return midKey > key ? this._rank(key, lo, mid - 1) : this._rank(key, mid + 1, hi);
  }

  rank(key) {
    return this._rank(key, 0, this.keys.length - 1);
  }

  put(key, value) {
    // 查找键，找到则更新，否在在当前位置插入新的元素
    const i = this.rank(key);
    if (i >= 0 && i < this.sz && this._keys[i] === key) {
      this.cost++;
      this._values[i] = value;
      return;
    }
    for (let j = this.sz; j > i; j--) {
      this.cost++;
      this._keys[j] = this._keys[j - 1];
      this._values[j] = this._values[j - 1];
    }
    this.cost++;
    this._keys[i] = key;
    this._values[i] = value;
    this.sz++;
  }

  get(key) {
    // get 操作会调用rank，使cost增加，但是题目统计的是put的成本
    const oldCost = this.cost;
    const i = this.rank(key);
    this.cost = oldCost;
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

  ceil(key) {
    const i = this.rank(key);
    return this._keys[i];
  }

  floor(key) {
    const i = this.rank(key);
    const rankKey = this._keys[i];
    return rankKey === key ? rankKey : this._keys[i - 1];
  }

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
  }
}

const path = require('path');

const fs = require('fs');

const {readLinesAsync} = require('../../util');

const tale = path.join(__dirname, '../../../test/input/algs4-data/tale.txt');

function wordCount(arr) {

  // 基于链表的符号表
  // 绿色散点(i,j):对于第i次put操作，成本为j
  // 红色折线(i,k):前i次put操作的累计平均比较成本为k

  // 基于平行数组的符号表
  // 黄色散点(i,j):对于第i次put操作，成本为j
  // 品红色折线(i,k):前i次put操作的累计平均比较成本为k

  const st1 = new SequentialSearchST();
  const st2 = new BinarySearchST();

  const scatterDataSet = [];
  const lineDataSet = [];

  let x = 0;

  for (const key of arr) {
    x++;
    for (const st of [st1, st2]) {

      const oldCost = st.cost;

      if (st.contains(key)) {
        st.put(key, st.get(key) + 1);
      } else {
        st.put(key, 1);
      }

      const nowCost = st.cost;
      const type = st.constructor.name;
      scatterDataSet.push({
        type,
        x,
        y: nowCost - oldCost + 1
      });
      lineDataSet.push({type, x, y: parseInt(nowCost / x)});
    }
  }
  const maxKey = -1;
  for (const st of [st1, st2]) {
    const oldCost = st.cost;

    st.put(maxKey, 0);

    const nowCost = st.cost;
    const type = st.constructor.name;
    scatterDataSet.push({
      type,
      x,
      y: nowCost - oldCost + 1
    });
    lineDataSet.push({type, x, y: parseInt(nowCost / x)});
  }

  const obj = {scatterDataSet, lineDataSet};

  fs.writeFileSync(path.join(__dirname, '38.g2.data.json'), JSON.stringify(obj));
}

(async () => {
  const words = []; // len 135643
  const lines = await readLinesAsync(tale);

  for (const line of lines) {
    const wordsInLine = line.split(/\s+/).map(x => x.trim()).filter(x => x.length > 0);
    for (const word of wordsInLine) {
      words.push(word);
    }
  }

  wordCount(words);
})();
