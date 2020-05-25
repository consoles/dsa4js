/**
 * @param {number[]} nums
 * @param {number} target
 * @return {number}
 */
var search = function (nums, target) {

//   由于题目说数字了无重复，举个例子：
// 1 2 3 4 5 6 7 可以大致分为两类，
// 第一类 2 3 4 5 6 7 1 这种，也就是 nums[start] <= nums[mid]。此例子中就是 2 <= 5。
// 这种情况下，前半部分有序。因此如果 nums[start] <=target<nums[mid]，则在前半部分找，否则去后半部分找。
// 第二类 6 7 1 2 3 4 5 这种，也就是 nums[start] > nums[mid]。此例子中就是 6 > 2。
// 这种情况下，后半部分有序。因此如果 nums[mid] <target<=nums[end]，则在后半部分找，否则去前半部分找。
// https://leetcode-cn.com/problems/search-in-rotated-sorted-array/solution/ji-bai-liao-9983de-javayong-hu-by-reedfan/

  let start = 0, end = nums.length - 1;
  while (start <= end) {
    const mid = start + ((end - start) >> 1);

    if (nums[mid] === target) return mid;

    // [start,mid]升序(注意小于等于)
    if (nums[mid] >= nums[start]) {
      if (target >= nums[start] && target <= nums[mid]) {
        // target在[start,mid]之间
        end = mid - 1;
      } else {
        // target不在[start,mid]之间，则一定在[mid+1,end]之间
        start = mid + 1;
      }
    } else {
      // 如果走到这个条件，说明start到mid经历了一个先升后降的过程，则[mid+1,end]一定是有序的
      if (target >= nums[mid] && target <= nums[end]) {
        start = mid + 1;
      } else {
        end = mid - 1;
      }
    }
  }
  return -1;
};

nums = [4, 5, 6, 7, 0, 1, 2], target = 0;
let index = search(nums, target);
