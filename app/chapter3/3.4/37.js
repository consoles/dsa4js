const RedBlackBST = require('../RedBlackBST');
const SequentialSearchST = require('../SequentialSearchST');

class SymbolTable {
  constructor(M) {
    this.initCap = 7;
    this.M = M || this.initCap;
    this.st = [];
    for (let i = 0; i < this.M; i++) {
      this.st[i] = new SequentialSearchST();
    }
    this.treeThreshold = 7;
  }

  _hash(key) {
    return (key.hashCode() & 0x7fffffff) % this.M;
  }

  put(key, value) {
    const index = this._hash(key);
    let table = this.st[index];
    if (table instanceof SequentialSearchST) {
      if (table.size >= this.treeThreshold) {
        const tree = new RedBlackBST();
        for (const key of table.keys()) {
          tree.put(key, table.get(key));
        }
        table = this.st[index] = tree;
      }
    }
    table.put(key, value);
  }

  get(key) {
    return this.st[this._hash(key)].get(key);
  }

  delete(key) {
    const index = this._hash(key);
    let table = this.st[index];
    if (table instanceof RedBlackBST) {
      if (table.size <= this.treeThreshold) {
        const list = new SequentialSearchST();
        for (const key of table.keys()) {
          list.put(key, table.get(key));
        }
      }
    }
    table.delete(key);
  }
}
