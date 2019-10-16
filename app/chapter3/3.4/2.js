class Node {
  constructor(key, value, next) {
    this.key = key;
    this.value = value;
    this.next = next || null;
  }
}

class SeparateChainingHashST {

  constructor(M = 97) {
    this.M = M;
    this.sts = [];
    this.N = 0;
  }

  * keys() {
    for (let cur of this.sts) {
      while (cur) {
        yield cur.key;
        cur = cur.next;
      }
    }
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
        cur.value = value;
        return;
      }
      prev = cur;
      cur = cur.next;
    }
    prev.next = new Node(key, value);
    this.N++;
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

for (const key of hashTable.keys()) {
  console.log(key);
}

debugger;
