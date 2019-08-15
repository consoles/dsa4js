class BinarySearchST {
  constructor() {
    this._keys = [];
    this._values = [];
    this.sz = 0;

    // 缓存，最近访问的地方很可能会被再次访问
    this.cacheIndex = 0;
  }

  _rank(key, lo, hi) {
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

    // 先查缓存
    const cacheIndex = this.cacheIndex;
    if (cacheIndex < this.sz && this._keys[cacheIndex] === key) {
      return this._values[cacheIndex];
    }

    // 未命中缓存则进行二分查找并更新缓存
    const i = this.rank(key);
    if (i >= 0 && i < this.sz && this._keys[i] === key) {
      this.cacheIndex = i;
      return this._values[i];
    }
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

class Node {
  constructor(key, value, next) {
    this.key = key;
    this.value = value;
    this.next = next || null;
  }
}

// cache 是一个结点类型的变量，代表一个键值对。
// 类似的，在顺序查找前先检查缓存，如果缓存未命中则更新缓存。
// 要注意的是如果缓存的结点被删除，需要将缓存置为 null。
class SequentialSearchST {

  constructor() {
    this.head = null;
    this.sz = 0;
    this.cache = null;
  }

  put(key, value) {
    if (!this.head) {
      this.head = new Node(key, value);
      this.sz++;
    } else {
      // 向后扫描，如果找到相同的元素就将key替换为value
      let cur = this.head;
      while (cur) {
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

  contains(key) {
    return this.get(key) !== null;
  }

  get(key) {
    if (!this.head) return null;

    const cache = this.cache;
    if (cache && cache.key === key) {
      return cache.value;
    }

    let cur = this.head;
    while (cur) {
      if (cur.key === key) {
        this.cache = cur;
        return cur.value;
      }
      cur = cur.next;
    }
    return null;
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
    if (parent.next === this.cache) {
      this.cache = null;
    }
    parent.next = parent.next.next;
    this.head = dummyHead.next;
    dummyHead.next = null;
    this.sz--;
  }

  delete2(key) {
    // 不用虚拟头结点的删除
    let parent = null;
    let cur = this.head;
    while (cur) {
      if (cur.key === key) {
        break;
      }
      parent = cur;
      cur = cur.next;
    }
    if (!cur) {
      throw new Error(`no such key ${key}`);
    }
    this.sz--;
    if (!parent) {
      if (this.cache === this.head) {
        this.cache = null;
      }
      this.head = cur.next;
    } else {
      parent.next = cur.next;
      if (cur === this.cache) {
        this.cache = null;
      }
    }
  }
}

const path = require('path');

function wordCount(st, arr) {
  let sum = 0;
  let testCount = 10;

  for (let i = 0; i < testCount; i++) {
    const start = Date.now();
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
    sum += (Date.now() - start);
  }

  return sum / testCount;
}

const {readLinesAsync} = require('../../util');

const tale = path.join(__dirname, '../../../test/input/algs4-data/tale.txt');

const SequentialSearchSTNoCache = require('../SequentialSearchST');
const BinarySearchSTNoCache = require('../BinarySearchST');

(async () => {
  const arr = [];
  const lines = await readLinesAsync(tale);
  for (const line of lines) {
    const words = line.split(/\s+/).map(x => x.trim()).filter(x => x.length > 0);
    for (const word of words) {
      arr.push(word);
    }
  }

  const st1 = new SequentialSearchSTNoCache();
  const st2 = new BinarySearchSTNoCache();
  const st3 = new SequentialSearchST();
  const st4 = new BinarySearchST();

  const t1 = wordCount(st1, arr);
  const t2 = wordCount(st2, arr);
  const t3 = wordCount(st3, arr);
  const t4 = wordCount(st4, arr);

  console.log('顺序查找表-无缓存', t1, '二分查找表-无缓存', t2, '顺序查找表-有缓存', t3, '二分查找表-有缓存', t4);
})();

// 顺序查找表-无缓存 89102.4 二分查找表-无缓存 616.4 顺序查找表-有缓存 57353.7 二分查找表-有缓存 428.6

// 启用缓存之后有30%的性能提升
