/**
 * @param {number} n
 * @return {number}
 */
const cache = {};
var climbStairs = function (n) {
  if (n === 1 || n === 2) return n;

  const dp = {1: 1, 2: 2};
  for (let i = 3; i <= n; i++) {
    dp[i] = dp[i - 1] + dp[i - 2];
  }
  return dp[n];
};

climbStairs(3);
