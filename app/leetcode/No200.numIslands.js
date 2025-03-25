/**
 * FloodFill
 *
 * @param {character[][]} grid
 * @return {number}
 */
var numIslands = function (grid) {

  // 注意：leetcode 矩阵中的是字符串 1，而不是数字 1
  // dfs
  // const mark = new Array(grid.length);
  //
  // for (let i = 0; i < grid.length; i++) {
  //   mark[i] = new Array(grid[i].length).fill(false);
  // }
  //
  // const d = [[0, 1], [1, 0], [-1, 0], [0, -1]];
  //
  // function dfs(i, j) {
  //   if (i >= grid.length || j >= grid[i].length) return;
  //   if (mark[i][j]) return;
  //
  //   mark[i][j] = true;
  //
  //   for (const [xOffset, yOffset] of d) {
  //     const newX = i + xOffset;
  //     const newY = j + yOffset;
  //     if (newX >= 0 && newX < grid.length && newY >= 0 && newY < grid[i].length && !mark[newX][newY]) {
  //       if (grid[newX][newY] == 1) {
  //         dfs(newX, newY);
  //       }
  //     }
  //   }
  // }
  //
  // let count = 0;
  //
  // for (let i = 0; i < grid.length; i++) {
  //   // 如果是岛屿中的一个点，并且没有被访问过，则进行dfs，dfs遍历会标记所选择的一整块陆地
  //   for (let j = 0; j < grid[i].length; j++) {
  //     if (!mark[i][j] && grid[i][j] == 1) {
  //       count++;
  //       dfs(i, j);
  //     }
  //   }
  // }
  // return count;

  // BFS
  // const mark = new Array(grid.length);
  //
  // for (let i = 0; i < grid.length; i++) {
  //   mark[i] = new Array(grid[i].length).fill(false);
  // }
  //
  // const d = [[0, 1], [1, 0], [-1, 0], [0, -1]];
  //
  // let count = 0;
  // for (let i = 0; i < grid.length; i++) {
  //   for (let j = 0; j < grid[i].length; j++) {
  //     if (!mark[i][j] && grid[i][j] == 1) {
  //       count++;
  //       const q = [];
  //       q.push([i, j]);
  //       mark[i][j] = true;
  //       while (q.length) {
  //         const [x, y] = q.shift();
  //         for (const [xOffset, yOffset] of d) {
  //           const newX = x + xOffset;
  //           const newY = y + yOffset;
  //           if (newX >= 0 && newX < grid.length && newY >= 0 && newY < grid[i].length && !mark[newX][newY] && grid[newX][newY] == 1) {
  //             q.push([newX, newY]);
  //             mark[newX][newY] = true;
  //           }
  //         }
  //       }
  //     }
  //   }
  // }
  // return count;

  // 不引入访问数组，直接修改grid数组,如果是陆地则floodfill一整块陆地
  const d = [[0, 1], [1, 0], [-1, 0], [0, -1]];

  function infect(i, j) {
    if (i >= grid.length || j >= grid[i].length) return;
    // 该位置被感染了
    grid[i][j] = 2;
    for (const [xOffset, yOffset] of d) {
      const newX = i + xOffset;
      const newY = j + yOffset;
      if (newX >= 0 && newX < grid.length && newY >= 0 && newY < grid[i].length && grid[newX][newY] == 1) {
        infect(newX, newY);
      }
    }
  }

  // 线性扫描二维网格，如果一个节点包含1，则以其为根节点进行DFS，在DFS的过程中感染与其相邻的所有大陆
  // 计算启动DFS的根节点的数量即可
  let count = 0;
  for (let i = 0; i < grid.length; i++) {
    for (let j = 0; j < grid[i].length; j++) {
      if (grid[i][j] == 1) {
        count++;
        infect(i, j);
      }
    }
  }
  return count;
};

grid = [
  [1, 1, 1],
  [0, 1, 0],
  [1, 1, 1],

];

numIslands(grid);
