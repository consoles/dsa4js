/**
 * @param {number[]} nums
 * @return {number}
 */
var maxProduct = function (nums) {
  // let max = Number.MIN_SAFE_INTEGER;
  //
  // const n = nums.length;
  //
  // for (let i = 0; i < n; i++) {
  //   let p = 1;
  //   for (let j = i; j < n; j++) {
  //     p *= nums[j];
  //     if (p > max) {
  //       max = p;
  //     }
  //   }
  // }
  //
  // return max;

  // 和53题【最大子数组的和】不一样的是，一个正数乘以负数变成2了最小值，即：最大值和最小值是互相转化的
  // dp[i][j]表示以nums[i]结尾的连续子数组的【最值】，j=0为最小值，j=1为最大值
  // const dp = new Array(nums.length);
  // for (let i = 0; i < dp.length; i++) {
  //   dp[i] = new Array(dp.length);
  // }
  // dp[0][0] = nums[0];
  // dp[0][1] = nums[0];
  // for (let i = 1; i < nums.length; i++) {
  //   const num = nums[i];
  //   if (num >= 0) {
  //     dp[i][0] = Math.min(dp[i - 1][0] * num, num);
  //     dp[i][1] = Math.max(dp[i - 1][1] * num, num);
  //   } else {
  //     // 当前是负数，则最小值和最大值对调
  //     dp[i][0] = Math.min(dp[i - 1][1] * num, num);
  //     dp[i][1] = Math.max(dp[i - 1][0] * num, num);
  //   }
  // }
  // // 只关心最大值
  // let max = dp[0][1];
  // for (let i = 1; i < nums.length; i++) {
  //   max = Math.max(max, dp[i][1]);
  // }
  // return max;

  // dp采用表格复用
  // 动态规划问题，常常是基于自底向上，用空间换时间的思想，通常是填表格。由于通常只关心最后一个状态值，或者在状态转移的时候，当前值仅仅参考了上一行的值，因此在填表的过程中表格可以复用，常用的技巧有：
  // 滚动数组（当前行只参考了上一行的时候只用2个表格完成全部计算）
  // 滚动变量（菲波那切数列）

  let prevMax = nums[0], prevMin = nums[0];

  // 滚动变量
  let curMax = 0, curMin = 0;

  let res = nums[0];
  for (let i = 1; i < nums.length; i++) {
    const num = nums[i];
    if (num >= 0) {
      curMax = Math.max(prevMax * num, num);
      curMin = Math.min(prevMin * num, num);
    } else {
      curMax = Math.max(prevMin * num, num);
      curMin = Math.min(prevMax * num, num);
    }
    res = Math.max(res, curMax);
    // 赋值滚动变量
    prevMax = curMax;
    prevMin = curMin;
  }
  return res;
};

maxProduct([2, 3, -2, 4]);
maxProduct([-2, 0, -1]);
