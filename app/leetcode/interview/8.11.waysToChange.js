/**
 * @param {number} n
 * @return {number}
 */
var waysToChange = function (n) {

  // dfs 超时

  // const coins = [25, 10, 5, 1];
  // let count = 0;
  //
  // function dfs(n, lastCoin) {
  //   if (n <= 0) {
  //     if (n === 0) count++;
  //     return;
  //   }
  //   for (const coin of coins) {
  //     if (lastCoin >= coin) {
  //       // 对于面值为6的硬币可能出现1,1,1,1,1,1 ; 5 + 1 ; 1 + 5三种，其中后后两种其实是同一种，我们只需要降序或者升序就行了
  //       dfs(n - coin, coin);
  //     }
  //   }
  // }
  //
  // dfs(n, Number.MAX_SAFE_INTEGER);
  // return count % 1000000007;

  // dp：完全背包问题,dp[i][j] 表示 i 种硬币组成面值为 j 时的方法数
  // dp[0][j] ：0 种硬币组成面值 j，不可能有方案，因此是 0，
  // dp[i][0] : 多种硬币组成面值 0，只有一种方案，就是一枚也不选
  // 状态转移方程：dp[i][j] = dp[i - 1][j] + dp[i][j - coins[i]]
  // 其中 dp[i - 1][j] 表示当前硬币不选，那么由 i - 1 种组成面值 j,dp[i][j - coins[i]]表示当前硬币选了，那么还需要组成面额为 j - coins[i], 这都是已要组成的面值大于当前硬币值为前提的。
  // 二维DP代码如下：
  // const coins = [1, 5, 10, 25];
  // const dp = new Array(5); // 多开一个位置，0空着不用
  // for (let i = 0; i < dp.length; i++) {
  //   dp[i] = new Array(n + 1).fill(0);
  //   dp[i][0] = 1;
  // }
  //
  // // 使用多少种硬币
  // for (let i = 1; i <= 4; i++) {
  //   // 组成的金额
  //   for (let j = 1; j <= n; j++) {
  //     // 要组成的面值比当前硬币金额小，该硬币不可以选择,只能由 i - 1 中硬币来组成面值 j
  //     if (j - coins[i - 1] < 0) {
  //       dp[i][j] = dp[i - 1][j] % 1000000007;
  //     } else {
  //       // 当前硬币可以不选，也可以选择
  //       dp[i][j] = (dp[i - 1][j] + dp[i][j - coins[i - 1]]) % 1000000007;
  //     }
  //   }
  // }
  // return dp[4][n];

  // 将二维dp压缩为1维
  // const coins = [1, 5, 10, 25];
  // const dp = new Array(n + 1).fill(0);
  // dp[0] = 1;
  // for (const coin of coins) {
  //   for (let i = 1; i <= n; i++) {
  //     if (i >= coin) {
  //       dp[i] += dp[i - coin];
  //     }
  //   }
  // }
  // return dp[n] % 1000000007;

  // DFS+缓存
  const coins = [25, 10, 5, 1];
  const cache = new Array(n + 1);
  for (let i = 0; i < cache.length; i++) {
    cache[i] = new Array(coins.length + 1).fill(0);
  }

  function dfs(n, cur) {
    if (n < 0) return 0;
    if (n === 0) return 1;
    if (n >= 0 && cache[n][cur] !== 0) {
      return cache[n][cur];
    }
    let res = 0;
    for (let i = cur; i < coins.length; i++) {
      if (n - coins[i] >= 0) {
        const c = cache[n - coins[i]][i];
        res += c !== 0 ? c : dfs(n - coins[i], i);
      }
    }
    return cache[n][cur] = res % 1000000007;
  }

  return dfs(n, 0);
};

waysToChange(10);
