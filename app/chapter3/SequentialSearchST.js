class Node {
  constructor(key, value, next) {
    this.key = key;
    this.value = value;
    this.next = next || null;
  }
}

// 符号表的实现：顺序查找表
class SequentialSearchST {

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

  contains(key) {
    return this.get(key) !== null;
  }

  get size() {
    return this.sz;
  }

  * keys() {
    if (!this.head) return;
    for (let cur = this.head; cur; cur = cur.next) {
      yield cur.key;
    }
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
      this.head = cur.next;
    } else {
      parent.next = cur.next;
    }
  }
}

module.exports = SequentialSearchST;

// const keys = 'SEARCHEXAMPLE'.split('');
// const st = new SequentialSearchST();
// for (let i = 0; i < keys.length; i++) {
//   st.put(keys[i], i);
// }
// debugger;
// console.log(st.keys, st.size);
//
// st.delete2('P');
// st.delete2('L');
// st.delete2('S');
