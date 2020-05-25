// 最大子序和

/**
 * @param {number[]} nums
 * @return {number}
 */
var maxSubArray = function (nums) {
  // 暴力法（超时）O(N^3)
  // let max = Number.MIN_SAFE_INTEGER;
  // for (let i = 0; i < nums.length; i++) {
  //   for (let j = i; j < nums.length; j++) {
  //     let sum = 0;
  //     for (let k = i;k <= j;k++) {
  //       sum += nums[k];
  //     }
  //     max = Math.max(max, sum(i, j));
  //   }
  // }
  // return max;

  // 改进暴力法O(N^2)
  // 每次我们都是重复计算了一部分子序列，即当我计算前两个时，第三次我还是会计算前两个在加第三个，这样就造成了O(N^3)，现在我们根据前一次的进行计算，那么将会减少一层循环
  // let sum = 0;
  // let max = Number.MIN_SAFE_INTEGER;
  // // 序列左端点
  // for (let i = 0; i < nums.length; i++) {
  //   sum = 0;
  //   // 序列右端点
  //   for (let j = i; j < nums.length; j++) {
  //     sum += nums[j]; // 这里就相当于每次根据前一次的序列来计算新的序列
  //     max = Math.max(max, sum);
  //   }
  // }
  //
  // return max;

  // 贪心法
  // 当我们加上一个正数时，和会增加；当我们加上一个负数时，和会减少。如果当前得到的和是个负数，那么这个和在接下来的累加中应该抛弃并重新清零，不然的话这个负数将会减少接下来的和。
  // let cur = nums[0];
  // let sum = nums[0];
  // for (let i = 1; i < nums.length; i++) {
  //   if (cur < 0) {
  //     // 当前数小于0 肯定会舍去（否则将会影响接下来的和），换为下一个数
  //     cur = nums[i];
  //   } else {
  //     // 如果当前数不小于0，那么他会对接下来的和有积极影响
  //     cur += nums[i];
  //   }
  //   if (cur > sum) {
  //     sum = cur;
  //   }
  // }
  // return sum;

  // 动态规划
  // dp[i]表示以i结尾的最大连续子序列和dp[i] = max(dp[i-1] + nums[i],nums[i])
  // const dp = [nums[0]];
  // for (let i = 1; i < nums.length; i++) {
  //   dp[i] = Math.max(dp[i - 1] + nums[i], nums[i]);
  // }
  // return Math.max.apply(null, dp);

  // 状态压缩的DP
  let max = nums[0];
  let sum = nums[0];
  for (let i = 1; i < nums.length; i++) {
    sum = Math.max(nums[i], sum + nums[i]);
    max = Math.max(sum, max);
  }
  return max;
};

maxSubArray([-2, 1, -3, 4, -1, 2, 1, -5, 4]);
