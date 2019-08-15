# 符号表

## 基于链表的顺序查找表

```js
class Node {
  constructor(key, value, next) {
    this.key = key;
    this.value = value;
    this.next = next || null;
  }
}

// 符号表的实现：顺序查找表
class SST {

  constructor() {
    this.head = null;
    this.sz = 0;
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

  get size() {
    return this.sz;
  }

  get keys() {
    const pathSums = [];
    if (!this.head) return pathSums;
    for (let cur = this.head; cur; cur = cur.next) {
      pathSums.push(cur.key);
    }
    return pathSums;
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

  delete2(key){
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
      this.head = cur.next;
    } else {
      parent.next = cur.next;
    }
  }
}
```

插入的时候从首节点开始扫描，如果碰到相等的键，则更新值为新的值，否则插入新的节点；查找的时候遍历链表；删除操作就是从链表中删除特定key的节点

## 基于有序平行数组的二分查找表

一对平行数组，一个存储key，一个存储value，保证key有序就可以使用数组的索引来高效实现`get`。

这份实现的核心是`rank`方法，它返回表中小于给定键的键的数量。对于`get`方法，只要给定的键存在于表中，rank方法就能精确地告诉我们在哪里可以找到它（如果找不到，那它肯定就不存在表中了）。

对于`put`方法，只要给定的键存在于表中，`rank`方法就能精确地告诉我们去哪里更新它的值，以及当键不在表中的时候将键存储到表的何处。我们将所有更大的键向右移动一格来腾出位置（从后向前移动）并将给定的键值分别插入到数组中的合适位置。

```js
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
    return this._rank(key, 0, this.size - 1);
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

  // keys(loKey, hiKey) {
  //   const pathSums = [];
  //   for (let i = this.rank(loKey); i < this.rank(hiKey); i++) {
  //     pathSums.push(this._keys[i]);
  //   }
  //   if (this.contains(hiKey)) {
  //     pathSums.push(hiKey);
  //   }
  //   return pathSums;
  // }

  get keys() {
    return this._keys.slice(0, this.size - 1);
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
```

尽管能够保证查找所需要的时间是对数级别，BinarySearchST的put和delete还是O(N)的，这意味着构造一个基于有序数组的符号表需要访问数组的次数是数组长度的平方级别（在大小为N的有序数组中插入一个新元素最坏情况下需要访问~2N次数组）。

二分搜索树结合了基于链表的顺序查找表和基于有序数组的二分查找表，结合了二分查找的效率和链表的灵活性。

## 单词计数中找出出现频率最高的所有单词

```js
// 单词计数

let maxKey = '';
counter.put(maxKey, 0);

const q = new Set();

// 传统算法，遍历符号表找出出现单词最多的次数，再次遍历符号表，找出等于最多次数的单词
// 这个算法利用队列的思想仅需要遍历一次符号表

for (const key of counter.keys()) {
  const count = counter.get(key);
  const maxCount = counter.get(maxKey);
  if (count > maxCount) {
    q.clear();
    q.add(key);
    maxKey = key;
  } else if (count === maxCount) {
    q.add(key);
  }
}

console.log(q);
```
