const assert = require('assert');

const BST = require('../BST');

class BST33 extends BST {
  checkRankSelect() {
    for (let i = 0; i < this.size; i++) {
      assert.deepStrictEqual(i, this.rank(this.select(i)));
    }
    for (const key of this.keys()) {
      assert.deepStrictEqual(key, this.select(this.rank(key)));
    }
  }
}

const bst = new BST33();

const keys = 'SEARCHEXAMPLE'.split('');

for (let i = 0; i < keys.length; i++) {
  bst.put(keys[i], i);
}

bst.checkRankSelect();
