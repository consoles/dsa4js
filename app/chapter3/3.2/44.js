const BST = require('../BST');

class Node {
  constructor(key, value, count) {
    this.key = key;
    this.value = value;
    this.left = null;
    this.right = null;
    this.count = count;
    this.height = 0;
  }
}

// put操作的成本定义为比较的次数

class BST44 extends BST {

  constructor() {
    super();
    this.compareCount = 0;
  }

  _put(node, key, value) {
    if (!node) return new Node(key, value, 1);
    const nodeKey = node.key;
    if (nodeKey < key) {
      this.compareCount++;
      node.left = this._put(node.left, key, value);
    } else if (node.key > key) {
      this.compareCount++;
      node.right = this._put(node.right, key, value);
    } else {
      node.value = value;
    }
    return node;
  }
}

const path = require('path');

const fs = require('fs');

const {readLinesAsync} = require('../../util');

const tale = path.join(__dirname, '../../../test/input/algs4-data/tale.txt');

function wordCount(words) {
  const bst = new BST44();

  let x = 0;
  let sum = 0;
  const scatterDataSet = [];
  const lineDataSet = [];
  for (const word of words) {
    x++;
    bst.compareCount = 0;
    if (bst.contains(word)) {
      bst.put(word, bst.get(word) + 1);
    } else {
      bst.put(word, 1);
    }
    const count = bst.compareCount;
    scatterDataSet.push({x, y: count});
    sum += count;
    lineDataSet.push({x, y: sum / x});
  }

  const obj = {scatterDataSet, lineDataSet};

  fs.writeFileSync(path.join(__dirname, '44.g2.data.json'), JSON.stringify(obj));
}

(async () => {
  const words = []; // len 135643
  const lines = await readLinesAsync(tale);

  for (const line of lines) {
    const wordsInLine = line.split(/\s+/).map(x => x.trim()).filter(x => x.length > 0);
    for (const word of wordsInLine) {
      words.push(word);
    }
  }

  wordCount(words);
})();
