/**
 * // This is the MountainArray's API interface.
 * // You should not implement it, or speculate about its implementation
 * function MountainArray() {
 *     @param {number} index
 *     @return {number}
 *     this.get = function(index) {
 *         ...
 *     };
 *
 *     @return {number}
 *     this.length = function() {
 *         ...
 *     };
 * };
 */

class MountainArray {
  constructor(arr) {
    this.arr = arr;
  }

  get(index) {
    return this.arr[index];
  }

  length() {
    return this.arr.length;
  }
}

/**
 * @param {number} target
 * @param {MountainArray} mountainArr
 * @return {number}
 */
var findInMountainArray = function (target, mountainArr) {
  const size = mountainArr.length();

  // 山顶元素所在的索引
  function findMountainTop(l, r) {
    while (l < r) {
      // 取左中位数，因为进入循环，数组一定有2个元素，则mid+1一定存在，数组不会越界
      const mid = (l + r) >> 1;
      if (mountainArr.get(mid) < mountainArr.get(mid + 1)) {
        // 当前的数比 右边的数小，则mid一定不是山顶元素
        l = mid + 1;
      } else {
        r = mid;
      }
    }
    // 根据题意，山顶元素一定存在，因此退出while循环的时候不用再单独判断
    return l;
  }


};
