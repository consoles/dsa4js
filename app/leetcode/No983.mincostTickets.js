/**
 * @param {number[]} days 出游的天数1-365
 * @param {number[]} costs costs数组为1天，7天，30天的票价
 * @return {number}
 */
var mincostTickets = function (days, costs) {

  // 记忆化搜索（日期变量型）
  const memo = new Array(366); // 1-365,空出0
  const daySet = new Set();
  for (const d of days) {
    daySet.add(d);
  }
  return dp(1);

  /**
   *
   * @param i 从第 i天开始到一年的结束，我们所需要花的钱
   * 考虑到一张通行证可以让我们在"接下来的若干天"进行旅行，我们"从后向前"倒着进行dp
   * 对于一年中的任意一天：
   * 如果这一天不是必须出行的日期，那我们可以贪心地选择不买。这是因为如果今天不用出行，那么也不必购买通行证，并且通行证越晚买越好。所以有 dp(i) = dp(i+1);
   * 如果这一天是必须出行的日期，我们可以选择买1,7,30天的通行证，如果我们购买了j天的通行证，那么接下来的j-1天，我们都不再需要购买通行证，只需要考虑第i+j天以后即可，因此有：
   * dp(i) = min{cost(j) + dp(i+j)},j ∈ {1,7,30}，其中 cost(j) 表示 j 天通行证的价格。
   * 为什么我们只需要考虑第 i+j天及以后呢？这里和第一条的贪心思路是一样的，如果我们需要购买通行证，那么一定越晚买越好，在握着一张有效的通行证的时候购买其它的通行证显然是不划算的。
   * 由于我们是倒着进行动态规划的，因此我们可以使用记忆化搜索，减少代码的编写难度。我们使用一个长度为 366 的数组（因为天数是 [1, 365]，而数组的下标是从 0 开始的）存储所有的动态规划结果，这样有的 dp(i)只会被计算一次（和普通的动态规划相同），时间复杂度不会增大。

   作者：LeetCode-Solution
   链接：https://leetcode-cn.com/problems/minimum-cost-for-tickets/solution/zui-di-piao-jie-by-leetcode-solution/
   来源：力扣（LeetCode）
   著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。
   */
  function dp(i) {
    if (i > 365) return 0;
    if (memo[i]) return memo[i];
    if (daySet.has(i)) {
      memo[i] = Math.min(
        dp(i + 1) + costs[0],
        dp(i + 7) + costs[1],
        dp(i + 30) + costs[2]
      );
    } else {
      memo[i] = dp(i + 1);
    }
    return memo[i];
  }
};


/**
 * 上面的方法需要遍历一年中的所有天数，无论days的长度为多少
 * 但是观察方法1的递推式，我们可以看到如果我们查询dp(i)，而第i天我们又不需要出行的话，那么dp函数会一直向后计算dp(i) = dp(i+1) = dp(i+2) 一直计算到一年结束或者有一天我们需要出行为止，那么我们其实可以直接跳过这些不需要出行的日期，直接找到下一个需要出行的日期:
 *
 * dp(i)表示从第days[i]天到最后的旅行计划的最小花费（注意：不再是第i天到最后的最小花费）。
 * j1是days[j1] >= days[i] + 1的最小下标,
 * j7是days[j7] >= days[i] + 7的最小下标，
 * j30是days[j30] >= days[i] + 30的最小下标
 * 那么有dp(i) = min(dp(j1) + costs[0],dp(j7) + costs[1],dp(j30) + costs[2])
 */
var mincostTickets = function (days, costs) {

  const durations = [1, 7, 30];

  const memo = new Array(days.length);

  function dp(i) {
    if (i >= days.length) {
      return 0;
    }
    if (memo[i]) {
      return memo[i];
    }
    let min = Number.MAX_SAFE_INTEGER;
    let j = i;
    for (let k = 0; k < 3; k++) {
      while (j < days.length && days[j] < days[i] + durations[k]) {
        j++;
      }
      min = Math.min(min, dp(j) + costs[k]);
    }
    return memo[i] = min;
  }

  return dp(0);
};
