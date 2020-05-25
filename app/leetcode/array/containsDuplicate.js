/**
 * @param {number[]} nums
 * @return {boolean}
 */
var containsDuplicate = function (nums) {
  // // 用set
  // const set = new Set();
  // for (const num of nums) {
  //   if (set.has(num)) {
  //     return true;
  //   }
  //   set.add(num);
  // }
  // return false;

  // 线性查找
  // for (let i = 1; i < nums.length; i++) {
  //   const num = nums[i];
  //   for (let j = 0; j < i; j++) {
  //     if (nums[j] === num) {
  //       return true;
  //     }
  //   }
  // }
  // return false;

  // 排序，然后比较相邻元素
  nums.sort((a, b) => a - b);
  for (let i = 0; i < nums.length - 1; i++) {
    if (nums[i] === nums[i + 1]) {
      return true;
    }
  }
  return false;
};
