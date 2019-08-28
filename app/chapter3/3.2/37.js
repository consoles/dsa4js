const BST = require('../BST');

class BST37 extends BST {
  /**
   * 按照层序打印（层序遍历，BFS）
   */
  printLevel() {
    const queue = [];
    let level = 0;
    queue.push([this.root, level]);
    while (queue.length > 0) {
      const [cur, level] = queue.shift();
      const {key, left, right} = cur;
      console.log(level, ' - ', key);
      if (left) {
        queue.push([left, level + 1]);
      }
      if (right) {
        queue.push([right, level + 1]);
      }
    }
  }
}

const bst = new BST37();
const keys = 'SEARCHEXAMPLE'.split('');
for (let i = 0; i < keys.length; i++) {
  bst.put(keys[i], i);
}
bst.printLevel();
