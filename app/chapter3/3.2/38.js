const BST = require('../BST');

// 如何按照层次的顺序进行打印

class BST38 extends BST {

  _maxLevel(node) {
    return !node ? 0 : Math.max(this._maxLevel(node.left), this._maxLevel(node.right)) + 1;
  }

  maxLevel() {
    return this._maxLevel(this.root);
  }

  isAllElementsNull(nodes) {
    return nodes.every(x => !x);
  }

  printWhiteSpace(count) {
    if (count > 0) {
      this.print(' '.repeat(count));
    }
  }

  print(str) {
    process.stdout.write(str);
  }

  _draw(nodes, level, maxLevel) {
    if (this.isEmpty() || this.isAllElementsNull(nodes)) {
      return;
    }
    const floor = maxLevel - level; // 层数越深，前面的空格越少
    const endgeLines = parseInt(2 ** (Math.max(0, floor - 1)));
    const firstSpaces = 2 ** floor - 1;
    const betweenSpaces = 2 ** (floor + 1) - 1;

    // 打印行开头的空格字符
    this.printWhiteSpace(firstSpaces);

    // 打印这一层级的所有元素,并构造下一层的完全二叉树
    const newNodes = [];
    for (const node of nodes) {
      if (node) {
        this.print(node.key);
        newNodes.push(node.left);
        newNodes.push(node.right);
      } else {
        this.printWhiteSpace(1);
        newNodes.push(null);
        newNodes.push(null);
      }
      this.printWhiteSpace(betweenSpaces); // 同一层2个节点之间的空格
    }
    this.print('\n');

    // 打印2层之间的空格,斜杠和反斜杠
    for (let i = 1; i <= endgeLines; i++) {
      for (let j = 0; j < nodes.length; j++) {
        this.printWhiteSpace(firstSpaces - i); // 每一行，前面的空格数减少1

        const node = nodes[j];
        // 完全二叉树对应的这个位置的节点不存在，打印"合适"的空格
        if (!node) {
          this.printWhiteSpace(2 * endgeLines + i + 1);
          continue;
        }
        // 有左孩子，打印 /
        if (node.left) {
          this.print('/');
        } else {
          this.printWhiteSpace(1);
        }

        this.printWhiteSpace(2 * i - 1); // 1,3,5,7...

        if (node.right) {
          this.print('\\'); // 注意转义
        } else {
          this.printWhiteSpace(1);
        }
        this.printWhiteSpace(2 * endgeLines - i);
      }
      this.print('\n');
    }

    this._draw(newNodes, level + 1, maxLevel);
  }

  draw() {
    const maxLevel = this.maxLevel();
    this._draw([this.root], 1, maxLevel);
  }
}

const bst = new BST38();
const keys = 'SEARCHEXAMPLE'.split('');
for (let i = 0; i < keys.length; i++) {
  bst.put(keys[i], i);
}
bst.draw();
