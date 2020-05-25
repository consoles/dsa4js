// 买卖股票的最佳时机 II

// 给定一个数组，它的第 i 个元素是一支给定股票第 i 天的价格。
//
// 设计一个算法来计算你所能获取的最大利润。你可以尽可能地完成更多的交易（多次买卖一支股票）。
//
// 注意：你不能同时参与多笔交易（你必须在再次购买前出售掉之前的股票）。
//
// 示例 1:
//
// 输入: [7,1,5,3,6,4]
// 输出: 7
// 解释: 在第 2 天（股票价格 = 1）的时候买入，在第 3 天（股票价格 = 5）的时候卖出, 这笔交易所能获得利润 = 5-1 = 4 。
//      随后，在第 4 天（股票价格 = 3）的时候买入，在第 5 天（股票价格 = 6）的时候卖出, 这笔交易所能获得利润 = 6-3 = 3 。
// 示例 2:
//
// 输入: [1,2,3,4,5]
// 输出: 4
// 解释: 在第 1 天（股票价格 = 1）的时候买入，在第 5 天 （股票价格 = 5）的时候卖出, 这笔交易所能获得利润 = 5-1 = 4 。
//      注意你不能在第 1 天和第 2 天接连购买股票，之后再将它们卖出。
//      因为这样属于同时参与了多笔交易，你必须在再次购买前出售掉之前的股票。
// 示例 3:
//
// 输入: [7,6,4,3,1]
// 输出: 0
// 解释: 在这种情况下, 没有交易完成, 所以最大利润为 0。

// https://leetcode-cn.com/articles/best-time-to-buy-and-sell-stock-ii/

/**
 * @param {number[]} prices
 * @return {number}
 */
var maxProfit = function (prices) {

  // 暴力法时间N^N，空间N（递归深度）

  // function _maxProfit(s) {
  //   if (s >= prices.length) return 0;
  //
  //   let max = 0;
  //
  //   for (let start = s; start < prices.length; start++) {
  //     let maxProfit = 0;
  //     const buyPrice = prices[start];
  //     for (let i = start + 1; i < prices.length; i++) {
  //       // 有盈利，可以卖
  //       const curProfit = prices[i] - buyPrice;
  //       if (curProfit > 0) {
  //         const profit = curProfit + _maxProfit(i + 1);
  //         if (profit > maxProfit) {
  //           maxProfit = profit;
  //         }
  //       }
  //     }
  //     if (maxProfit > max) {
  //       max = maxProfit;
  //     }
  //   }
  //   return max;
  // }
  //
  // return _maxProfit(0);

  // 波峰波谷法，时间N,空间1
  // let i = 0;
  // let valley = prices[0]; // 波谷
  // let peek = prices[0]; // 波峰
  // let maxProfit = 0;
  // while (i < prices.length - 1) {
  //   while (i < prices.length - 1 && prices[i] >= prices[i + 1]) {
  //     i++;
  //   }
  //   valley = prices[i];
  //   while (i < prices.length - 1 && prices[i] <= prices[i + 1]) {
  //     i++;
  //   }
  //   peek = prices[i];
  //   maxProfit += peek - valley;
  // }
  // return maxProfit;

  // 简单一次遍历，时间N，空间1
  // 上面方法的改进
  let maxProfit = 0;
  for (let i = 1; i < prices.length; i++) {
    if (prices[i] > prices[i - 1]) {
      maxProfit += prices[i] - prices[i - 1];
    }
  }
  return maxProfit;
};

let prices = [7, 1, 5, 3, 6, 4];
let ret = maxProfit(prices);
console.log(ret);
