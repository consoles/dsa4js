const mathjs = require('mathjs');

const BST = require('../BST');

class BST39 extends BST {
  constructor() {
    super();

    this.compareCount = 0;
  }

  // 禁用缓存
  get(key) {
    return this._get(this.root, key);
  }

  _get(node, key) {
    if (!node) return null;
    const nodeKey = node.key;
    if (nodeKey === key) {
      this.compareCount++;
      return node.value;
    }
    this.compareCount++;
    return key > nodeKey ? this._get(node.right, key) : this._get(node.left, key);
  }
}

const {randomArray} = require('../../util');

// 平均差
function meanAbsoluteDeviation(arr) {
  const mean = mathjs.mean(arr);
  return mathjs.sum(arr.map(x => mathjs.abs(x - mean))) / arr.length;
}

function test(n) {
  const arr = randomArray(1, n, n);
  const bst = new BST39();
  for (let i = 0; i < arr.length; i++) {
    bst.put(arr[i], i);
  }
  const compareCounts = [];
  for (const item of arr) {
    bst.compareCount = 0;
    bst.get(item);
    compareCounts.push(bst.compareCount);
  }

  console.log('n = ', n, '1.39LgN-1.85', 1.39 * Math.log2(n) - 1.85,'均值', mathjs.mean(compareCounts), '平均差', meanAbsoluteDeviation(compareCounts), '标准差', mathjs.std(compareCounts));
}

for (let n = 1e4; n <= 1e6; n *= 10) {
  test(n);
}

// n =  10000 1.39LgN-1.85 16.61992020757373 均值 15.8082 平均差 3.145146640000082 标准差 3.9554490665071924
// n =  100000 1.39LgN-1.85 21.237400259467165 均值 20.69114 平均差 3.7428218651991068 标准差 4.749003161791922
// n =  1000000 1.39LgN-1.85 25.8548803113606 均值 23.929965 平均差 3.7396127468716545 标准差 4.710425701234536
