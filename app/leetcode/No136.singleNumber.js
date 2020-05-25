/**
 * @param {number[]} nums
 * @return {number}
 */
var singleNumber = function(nums) {
  // 异或
  let x = 0;
  for (const num of nums) {
    x = x ^ num;
  }
  return x;
};
