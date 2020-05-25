/**
 * @param {number[]} nums
 * @param {number} target
 * @return {number}
 */
var searchInsert = function (nums, target) {

  let l = 0, r = nums.length - 1;
  while (l <= r) {
    const mid = l + parseInt((r - l) / 2);
    const item = nums[mid];
    if (item === target) {
      return mid;
    }
    if (item < target) {
      l = mid + 1;
    } else {
      r = mid - 1;
    }
  }
  return l;
};

searchInsert([1,3,5,6],2);
