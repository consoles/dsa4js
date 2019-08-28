const _ = require('lodash');

const BST = require('../BST');
const {randomArray} = require('../../util');

class BST42 extends BST {
  _delete(node, key) {
    if (!node) {
      return null;
    }
    const nodeKey = node.key;
    if (key < nodeKey) {
      node.left = this._delete(node.left, key);
    } else if (key > nodeKey) {
      node.right = this._delete(node.right, key);
    } else {
      if (!node.left) return node.right;
      if (!node.right) return node.left;
      const usePrev = Math.random() > .5;
      const t = node;
      // 随机选择前驱或者后继元素替代当前节点
      if (usePrev) {
        node = this._max(node.left);
        node.left = this._deleteMax(t.left);
        node.right = t.right;
      } else {
        node = this._min(node.right);
        node.right = this._deleteMin(t.right);
        node.left = t.left;
      }
    }
    return node;
  }
}

function test(n, bst) {

  const arr = randomArray(1, n, n);
  for (let i = 0; i < arr.length; i++) {
    bst.put(arr[i], i);
  }

  const beforePathSum = bst.pathSum;

  let testCount = n ** 2;

  for (let i = 0; i < testCount; i++) {
    // 删除随机键
    const rand = _.random(0, bst.size - 1);
    const key = bst.select(rand);
    bst.delete(key);
    // 插入随机键
    bst.put(key, i);
  }

  const afterPathSum = bst.pathSum;

  const incr = afterPathSum - beforePathSum;

  console.log(bst.constructor.name, 'n = ', n, 'before', beforePathSum, 'after', afterPathSum, 'incr = ', incr, 'sqrt(n) = ', Math.sqrt(n));
}

for (const n of [1e2, 1e3, 1e4]) {
  test(n, new BST());
  test(n, new BST42());
}

// BST n =  100 before 341 after 407 incr =  66 sqrt(n) =  10
// BST42 n =  100 before 471 after 394 incr =  -77 sqrt(n) =  10
// BST n =  1000 before 7398 after 8930 incr =  1532 sqrt(n) =  31.622776601683793
// BST42 n =  1000 before 7403 after 5941 incr =  -1462 sqrt(n) =  31.622776601683793
// BST n =  10000 before 97008 after 243295 incr =  146287 sqrt(n) =  100
// BST42 n =  10000 before 100009 after 87734 incr =  -12275 sqrt(n) =  100

// 书上说和n的平方根成正比，我真的没发现！！，感觉好像是平方关系
