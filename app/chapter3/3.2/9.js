// 给定N = 2，3，4，5，6输出用N个键所能构成的所有二分搜索树

// 方法：
// 1.生成序列的全排列
// 2.根据这些全排列序列不断进行put操作，生成BST，对BST进行去重（结构一样）

const _ = require('lodash');

function getAllAssignment(arr) {
  const res = [];

  function dfs(result) {
    if (result.length === arr.length) {
      res.push(result);
      return result;
    }
    for (let i = 0; i < arr.length; i++) {
      if (result.indexOf(arr[i]) === -1) {
        dfs(result.concat(arr[i]));
      }
    }
  }

  dfs([]);
  return res;
}

class Node {
  constructor(key) {
    this.key = key;
    this.left = null;
    this.right = null;
  }
}

class BST {
  constructor() {
    this.root = null;
  }

  _put(node, key) {
    if (!node) return new Node(key);
    if (key < node.key) {
      node.left = this._put(node.left, key);
    } else {
      node.right = this._put(node.right, key);
    }
    return node;
  }

  put(key) {
    this.root = this._put(this.root, key);
  }

  _height(node) {
    if (!node) return -1;
    return Math.max(this._height(node.left), this._height(node.right)) + 1;
  }

  height() {
    return this._height(this.root);
  }

  toString() {
    return JSON.stringify(this.root);
  }
}

const res = getAllAssignment([1, 2, 3, 4, 5, 6]);
const bsts = [];
for (const seq of res) {
  const bst = new BST();
  for (let item of seq) {
    bst.put(item);
  }
  bsts.push(bst);
}

const uniq = _.uniqBy(bsts, bst => bst.toString());
debugger;
