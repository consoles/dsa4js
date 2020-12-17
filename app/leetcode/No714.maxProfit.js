/**
 * @param {number[]} prices
 * @param {number} fee
 * @return {number}
 */
var maxProfit = function (prices, fee) {
    // 每天可以选择买入或者卖出，持有的时候不能再次买入，每笔交易有固定手续费，求最大利润
    // 状态转移方程：dp[n][2]
    // dp[i][0]表示第i天不持有可以获得的最大利润
    // dp[i][1]表示第i天持有可以获得的最大利润（注意是第i天持有，而不是第i天买入）
    // 对于今天不持有，可以有两种状态转化而来：1，昨天也不持有；2，昨天持有，今天卖出。两者取最大值
    // dp[i][0] = max(dp[i-1][0],dp[i-1][1] + prices[i] - fee)
    // 对于今天持有，可以有两种状态转化而来：1，昨天也持有；2，昨天不持有，今天买入，两者取最大值
    // dp[i][1] = max(dp[i-1][1],dp[i-1][0] - prices[i])
    // 给定第0天，不持有：dp[0][0] = 0
    // 持有：dp[0][1] = -prices[0] // 即：花了prices[0]买入
    const n = prices.length;
    // const dp = new Array(n);
    // for (let i = 0; i < n; i++) {
    //     dp[i] = new Array(2);
    // }
    // dp[0][0] = 0;
    // dp[0][1] = -prices[0];
    // for (let i = 1; i < n; i++) {
    //     dp[i][0] = Math.max(dp[i - 1][0], dp[i - 1][1] + prices[i] - fee);
    //     dp[i][1] = Math.max(dp[i - 1][1], dp[i - 1][0] - prices[i]);
    // }
    // return dp[n - 1][0];

    // 降维，因为dp[i]只依赖dp[i-1]
    // let dp0 = 0;
    // let dp1 = -prices[0];
    // for (let i = 1; i < n; i++) {
    //     const tmp = dp0;
    //     dp0 = Math.max(tmp, dp1 + prices[i] - fee);
    //     dp1 = Math.max(dp1, tmp - prices[i]);
    // }
    // return dp0;

    // 贪心算法
    let buy = prices[0] + fee;
    let profit = 0;
    for (let i = 1; i < n; i++) {
        if (prices[i] + fee < buy) {
            // 如果当前股票价格prices[i]加上手续费fee小于buy，那么与其使用buy的价格买股票，不如以prices[i] + fee的价格购买股票，因此将buy的值更新为prices[i]+fee
            buy = prices[i] + fee;
        } else if (prices[i] > buy) {
            // 直接卖出股票可以得到prices[i] - buy的收益，但是实际上我们此时卖出股票可能不是全局最优的
            // 因此我们可以提供一个反悔操作，看成当前手上拥有一支买入价格为prices[i]的股票，将buy更新为prices[i]
            // 这样一来，如果下一天股票价格继续上升，我们将会获得prices[i+1] - prices[i]的收益，加上这一天prices[i] - buy的收益，恰好等于这一天不做任何操作，而在下一天卖出股票的收益：当我们卖出股票的时候我们就立即获得了以相同价格并且免手续费买入一只股票的权利
            profit += prices[i] - buy;
            buy = prices[i];
        }
        // 对于其他情况，prices[i]落在区间[buy-fee,buy]内，它的价格没有低到放弃我们手上的股票去选择买入它，有没有高到我们可以卖出它获得收益，不做任何操作
    }
    return profit;
};

prices = [1, 3, 2, 8, 4, 9], fee = 2
const res = maxProfit(prices, fee);
debugger