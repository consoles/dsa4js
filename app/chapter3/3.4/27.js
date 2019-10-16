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
  }

  put(key, value) {
    if (!this.head) {
      this.head = new Node(key, value);
      this.sz++;
    } else {
      // 向后扫描，如果找到相同的元素就将key替换为value
      let cur = this.head;
      while (cur) {
        if (cur.key.equals(key)) {
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
      if (cur.key.equals(key)) {
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

}

class SeparateChainingHashST {
  constructor(M = 3) {
    this.M = M; // 散列表大小（存放散列索引的数组大小）
    this.sts = new Array(M);

    for (let i = 0;i < M;i++) {
      this.sts[i] = [new SequentialSearchST(), new SequentialSearchST()];
    }
  }

  get size(){
    let count = 0;
    for (const st of this.sts) {
      for (const head of st) {
        count+= head.size;
      }
    }
    return count;
  }

  _hash(key) {
    return (key.hashCode() & 0x7fffffff) % this.M;
  }

  * keys() {
    for (const st of this.sts) {
      for (let cur of st) {
        while (cur) {
          yield cur.key;
          cur = cur.next;
        }
      }
    }
  }

  get(key) {
    const sts = this.sts[this._hash(key)];
    let container = sts[0];
    if (sts[1].size < container.size) {
      container = sts[1];
    }
    return container.get(key);
  }

  put(key, value) {
    const sts = this.sts[this._hash(key)];
    let container = sts[0];
    if (sts[1].size < container.size) {
      container = sts[1];
    }
    container.put(key, value);
  }
}

class Item1 {
  constructor(key, index) {
    this.key = key;
    this.index = index;
  }

  hashCode() {
    return (this.key.charCodeAt(0) - 'A'.charCodeAt(0)) * 11;
  }

  equals(other) {
    return this.key === other.key;
  }
}

class Item2 extends Item1 {
  hashCode() {
    return (this.key.charCodeAt(0) - 'A'.charCodeAt(0)) * 17;
  }
}

const keys = 'EASYQUTION'.split('');
const data1 = keys.map((x, i) => new Item1(x, i));
const data2 = keys.map((x, i) => new Item2(x, i));

let st = new SeparateChainingHashST();
for (let i = 0; i < data1.length; i++) {
  st.put(data1[i], i);
}

// 0 : Y -> A
// 0 : S
// 1 : O -> U
// 1 : I
// 2 : T -> E
// 2 : N -> Q

st = new SeparateChainingHashST();
for (let i = 0; i < data2.length; i++) {
  st.put(data2[i], i);
}
debugger;

// 0:Y->A
// 0:S
// 1:O->U
// 1:I
// 2:T->E
// 2:N->Q

// 两种hash函数构造的hash表形状一样
