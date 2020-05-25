/**
 * @param {number[]} prices
 * @return {number}
 */
var maxProfit = function (prices) {
  if (!prices || prices.length <= 1) return 0;
  // let max = 0;
  // for (let i = 0; i < prices.length - 1; i++) {
  //   const buyPrice = prices[i];
  //   for (let j = i + 1; j < prices.length; j++) {
  //     const sellPrice = prices[j];
  //     max = Math.max(max, sellPrice - buyPrice);
  //   }
  // }
  // return max;

  // 简化为一次遍历
  // let min = Number.MAX_SAFE_INTEGER;
  // let maxProfit = 0;
  // for (const price of prices) {
  //   if (price < min) {
  //     min = price; // 第一天到当前天的最小值
  //   } else {
  //     // if (price - min > maxProfit) {
  //     //   maxProfit = price - min;
  //     // }
  //     maxProfit = Math.max(price - min, maxProfit);
  //   }
  // }
  // return maxProfit;

  // 单调栈
  let maxProfit = 0;
  const stack = [];
  for (const price of prices) {
    while (stack.length && stack[stack.length - 1] > price) {
      maxProfit = Math.max(maxProfit, stack[stack.length - 1] - stack[0]);
      stack.pop();
    }
    stack.push(price);
  }
  return maxProfit;
};

maxProfit([2, 1]);
