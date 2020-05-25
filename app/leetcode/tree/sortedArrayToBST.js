/**
 * Definition for a binary tree node.
 * function TreeNode(val) {
 *     this.val = val;
 *     this.left = this.right = null;
 * }
 */

function TreeNode(val) {
  this.val = val;
  this.left = this.right = null;
}

/**
 * @param {number[]} nums
 * @return {TreeNode}
 */
var sortedArrayToBST = function (nums) {
  // 思路类似二分搜索
  function buildBST(l, r) {
    if (l > r) return null;

    // 如果区间为偶数始终选择左边的值作为根节点元素
    // const mid = l + parseInt((r - l) / 2);
    // 如果区间为偶数，则选择右边值作为根节点
    // const mid = l + Math.ceil((r - l) / 2);
    // 如果区间为偶数，则随机选择左边或者右边作为根节点
    const mid = l + (Math.random() > .5 ? Math.ceil((r - l) / 2) : Math.floor((r - l) / 2));
    const node = new TreeNode(nums[mid]);
    node.left = buildBST(l, mid - 1);
    node.right = buildBST(mid + 1, r);
    return node;
  }

  return buildBST(0, nums.length - 1);
};

arr = [-10, -3, 0, 5, 9];
root = sortedArrayToBST(arr);
debugger;
