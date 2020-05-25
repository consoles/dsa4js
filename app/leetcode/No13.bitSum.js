// 面试题13. 机器人的运动范围

// 地上有一个m行n列的方格，从坐标 [0,0] 到坐标 [m-1,n-1] 。一个机器人从坐标 [0, 0] 的格子开始移动，它每次可以向左、右、上、下移动一格（不能移动到方格外），也不能进入行坐标和列坐标的数位之和大于k的格子。例如，当k为18时，机器人能够进入方格 [35, 37] ，因为3+5+3+7=18。但它不能进入方格 [35, 38]，因为3+5+3+8=19。请问该机器人能够到达多少个格子？
//
// 来源：力扣（LeetCode）
// 链接：https://leetcode-cn.com/problems/ji-qi-ren-de-yun-dong-fan-wei-lcof
//   著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。
//
// 示例 1：
//
// 输入：m = 2, n = 3, k = 1
// 输出：3
// 示例 1：
//
// 输入：m = 3, n = 1, k = 0
// 输出：1
// 提示：
//
// 1 <= n,m <= 100
// 0 <= k <= 20
//
// 来源：力扣（LeetCode）
// 链接：https://leetcode-cn.com/problems/ji-qi-ren-de-yun-dong-fan-wei-lcof
//   著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。

function bitSum(num) {
  let sum = 0;
  while (num) {
    sum += num % 10;
    num = parseInt(num / 10);
  }
  return sum;
}

function getSum(num1, num2) {
  return bitSum(num1) + bitSum(num2);
}

/**
 * @param {number} m
 * @param {number} n
 * @param {number} k
 * @return {number}
 */
var movingCount = function (m, n, k) {

  const marked = new Array(m);
  for (let i = 0; i < m; i++) {
    marked[i] = new Array(n).fill(false);
  }

  // 搜索方向可以简化为 向右和向下 不必要再进行向上和向左进行搜索

  // DFS
  // function dfs(x, y, sum) {
  //   if (x >= m || y >= n) return 0;
  //   if (marked[x][y]) return 0;
  //   if (sum > k) return 0;
  //   marked[x][y] = true;
  //   // 向右或者向下dfs
  //   return 1 + dfs(x + 1, y, getSum(x + 1, y))
  //     + dfs(x, y + 1, getSum(x, y + 1));
  // }
  //
  // return dfs(0, 0, 0);

  // BFS
  const queue = [[0, 0]];
  let count = 0;
  marked[0][0] = true;
  while (queue.length) {
    const [x, y] = queue.shift();
    count++;
    // 向右
    if (x + 1 < m && !marked[x + 1][y] && getSum(x + 1, y) <= k) {
      queue.push([x + 1, y]);
      marked[x + 1][y] = true;
    }
    // 向下
    if (y + 1 < n && !marked[x][y + 1] && getSum(x, y + 1) <= k) {
      queue.push([x, y + 1]);
      marked[x][y + 1] = true;
    }
  }
  return count;
};

let ret = movingCount(16, 8, 4);
console.log(ret);
