class Node {
  constructor(key, next) {
    this.key = key;
    this.next = next;
  }
}

class SequentialSearchSet {
  constructor() {
    this.head = null;
    this.sz = 0;
  }

  add(key) {
    if (!this.head) {
      this.head = new Node(key);
      this.sz++;
    } else {
      let cur = this.head;
      while (cur) {
        if (cur.key === key) {
          return;
        }
        cur = cur.next;
      }
      this.head = new Node(key, this.head);
      this.sz++;
    }
  }

  delete(key) {
    let prev = null;
    let cur = this.head;
    while (cur) {
      if (cur.key === key) {
        break;
      }
      prev = cur;
      cur = cur.next;
    }
    if (!cur) {
      throw new Error(`no such element ${key}`);
    }
    if (!prev) {
      this.head = cur.next;
    } else {
      prev.next = cur.next;
    }
  }

  contains(key) {
    let cur = this.head;
    while (cur) {
      if (cur.key === key) {
        return true;
      }
      cur = cur.next;
    }
    return false;
  }

  isEmpty() {
    return this.size === 0;
  }

  get size() {
    return this.sz;
  }

  toString() {
    const items = [];
    let cur = this.head;
    while (cur) {
      items.push(cur.key);
      cur = cur.next;
    }
    return `size = ${this.size},{ ${items.join(',')} }`;
  }
}

const s = new SequentialSearchSet();

for (const item of [1,1,2,3,4,5,6]) {
  s.add(item);
}
console.log(s.toString());
s.delete(1);
console.log(s.toString());
console.log(s.contains(1), s.contains(2));
console.log(s.toString());
