// 为每个键值对添加一个整型变量，将其值设置为插入该键值对的时候散列表中元素的数量。实现一个方法，将该变量的值大于特定整数k的键（及其对应的值）全部删除
// 这个额外的功能在为编译器实现符号表的时候很有用

class Node {
  constructor(key, value, next) {
    this.key = key;
    this.value = value;
    this.count = 0;
    this.next = next || null;
  }
}

class SeparateChainingHashST {

  constructor(M = 3) {
    this.M = M;
    this.sts = [];
    this.N = 0;
  }

  get size() {
    return this.N;
  }

  _hash(key) {
    return (key.hashCode() & 0x7fffffff) % this.M;
  }

  put(key, value) {
    const hash = this._hash(key);
    let head = this.sts[hash];
    if (!head) {
      head = new Node(key, value);
      this.N++;
      this.sts[hash] = head;
      return;
    }
    let cur = head;
    let prev = null;
    while (cur) {
      if (cur.key.equals(key)) {
        cur.count++;
        cur.value = value;
        return;
      }
      prev = cur;
      cur = cur.next;
    }
    prev.next = new Node(key, value);
    this.N++;
  }

  deleteMore(k) {
    for (let i = 0; i < this.M; i++) {
      let cur = this.sts[i];
      if (!cur) {
        continue;
      }
      let prev = null;
      while (cur) {
        const next = cur.next;
        let hasPrev = true;
        if (cur.count > k) {
          if (!prev) {
            // 删除的是否是头结点需要单独判断
            this.sts[i] = next;
            hasPrev = false;
          } else {
            prev.next = next;
          }
          this.N--;
        }
        prev = hasPrev ? cur : null;
        cur = next;
      }
    }
  }

  get(key) {
    let cur = this.sts[this._hash(key)];
    while (cur) {
      if (cur.key.equals(key)) {
        return cur.value;
      }
      cur = cur.next;
    }
    return null;
  }

  delete(key) {
    const hash = this._hash(key);
    let prev = null;
    let cur = this.sts[hash];
    if (!cur) return;

    let shouldDelete = false;

    while (cur) {
      if (cur.key.equals(key)) {
        shouldDelete = true;
        break;
      }
      prev = cur;
      cur = cur.next;
    }
    if (!shouldDelete) return;
    if (!prev) {
      this.sts[hash] = cur.next;
    } else {
      prev.next = cur.next;
    }
    this.N--;
  }
}

class CharEntry {
  constructor(key, value) {
    this.key = key;
    this.value = value;
  }

  hashCode() {
    return this.key.charCodeAt(0);
  }

  equals(other) {
    return this.key === other.key;
  }
}

const hashTable = new SeparateChainingHashST();

const items = 'SEARCHEXAMPLE'.split('').map((x, index) => new CharEntry(x, index));

for (const item of items) {
  hashTable.put(item, item.value);
}

hashTable.deleteMore(0);
debugger;
