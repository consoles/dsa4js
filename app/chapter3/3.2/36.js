const BST = require('../BST');

// keys方法的非递归实现

class BST36 extends BST {

  * rangeKeys(lo, hi) {
    if (this.isEmpty()) return;
    if (lo > hi) return;
    return yield* this._keys(this.root, lo, hi);
  }

  * _keys(node, lo, hi) {
    if (!node) return;
    const stack = [];
    let cur = this.root;
    while (cur || stack.length > 0) {
      if (cur) {
        stack.push(cur);
        if (cur.key < lo) {
          cur = null; // 手动命中else
        } else {
          cur = cur.left;
        }
      } else {
        cur = stack.pop();
        const key = cur.key;

        if (key > hi) {
          break;
        }

        if (key >= lo) {
          yield key;
        }
        cur = cur.right;
      }
    }
  }

  * keys() {
    if (this.isEmpty()) return;
    return yield* this._keys(this.root, this.min, this.max);
  }
}

const bst = new BST36();
const keys = 'SEARCHEXAMPLE'.split('');
for (let i = 0; i < keys.length; i++) {
  bst.put(keys[i], i);
}

for (const key of bst.rangeKeys('H','P')) {
  console.log(key);
}
