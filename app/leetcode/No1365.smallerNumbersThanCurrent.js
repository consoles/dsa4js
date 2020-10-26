/**
 * @param {number[]} nums
 * @return {number[]}
 */
var smallerNumbersThanCurrent = function (nums) {
  // 方法1：优化的二重循环
  // const sortedNums = nums.slice().sort((a, b) => a - b);
  // const arr = [];
  // for (let i = 0; i < nums.length; i++) {
  //   const num = nums[i];
  //   let count = 0;
  //   let j = 0;
  //   while (j < sortedNums.length) {
  //     if (sortedNums[j] < num) {
  //       count++;
  //     }
  //     j++;
  //   }
  //   arr.push(count);
  // }
  // return arr;

  // 方法2：计数排序
  const cnt = new Array(101).fill(0); // 0 <= nums[i] <= 100
  const n = nums.length;
  // 建立一个频次数组 cntcnt ，cnt[i] 表示数字i出现的次数。那么对于数字i而言，小于它的数目就为 cnt[0...i-1]的总和。
  for (let i = 0; i < n; i++) {
    cnt[nums[i]]++;
  }
  for (let i = 1; i <= 100; i++) {
    cnt[i] += cnt[i - 1];
  }
  const ret = [];
  for (let i = 0; i < n; i++) {
    ret.push(nums[i] > 0 ? cnt[nums[i] - 1] : 0);
  }
  return ret;
};

let res = smallerNumbersThanCurrent([8, 1, 2, 2, 3]);
res = smallerNumbersThanCurrent([6, 5, 4, 8]);
res = smallerNumbersThanCurrent([7, 7, 7, 7]);
debugger
