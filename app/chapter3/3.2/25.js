// 完美平衡的BST

class Node {
  constructor(key) {
    this.key = key;
    this.left = null;
    this.right = null;
  }
}

function sortedArrayToBST(nums) {
  function _helper(lo, hi) {
    if (lo > hi) {
      return null;
    }
    const mid = parseInt((lo + hi) / 2);
    const node = new Node(nums[mid]);
    node.left = _helper(lo, mid - 1);
    node.right = _helper(mid + 1, hi);
    return node;
  }

  return _helper(0, nums.length - 1);
}

const ret = sortedArrayToBST([1, 2, 3, 4, 5, 6, 7]);
debugger;
