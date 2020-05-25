/**
 * @param {number[]} nums
 * @return {number}
 */
var rob = function (nums) {
  const n = nums.length;
  if (n === 0) return 0;
  const dp = new Array(n);
  dp[0] = 0;
  dp[1] = nums[0];
  for (let i = 2; i <= n; i++) {
    dp[i] = Math.max(dp[i - 1], dp[i - 2] + nums[i - 1]);
  }
  return dp[n];
};

rob([2, 7, 9, 3, 1]);
