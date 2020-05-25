/**
 * @param {number[][]} matrix
 * @return {number[][]}
 */
var updateMatrix = function (matrix) {

  if (!matrix || matrix.length === 0) return matrix;

  const m = matrix.length;
  const n = matrix[0].length;

  // const res = [];
  // const visited = [];
  // for (let i = 0; i < m; i++) {
  //   res.push(new Array(n));
  //   visited.push(new Array(n).fill(false));
  // }
  //
  // const d = [[0, -1], [0, 1], [1, 0], [-1, 0]]; // 4个方向数组
  //
  // // bfs
  // // 1.将等于0的位置直接放入结果集并入队作为广度优先的起点(多起点的BFS)
  // const q = [];
  // for (let i = 0; i < m; i++) {
  //   for (let j = 0; j < n; j++) {
  //     if (matrix[i][j] === 0) {
  //       res[i][j] = 0;
  //       visited[i][j] = true;
  //       q.push([i, j]);
  //     }
  //   }
  // }
  //
  // // 2.多起点的广度优先
  // while (q.length) {
  //   const [x, y] = q.shift();
  //   for (const [offsetX, offsetY] of d) {
  //     const newX = x + offsetX;
  //     const newY = y + offsetY;
  //     // 没有访问过的地方一定是1，因为第一步中值为0的位置全部被标记了
  //     if (newX >= 0 && newX < m && newY >= 0 && newY < n && !visited[newX][newY]) {
  //       // (newX,newY) 是 (x,y)的相邻节点
  //       res[newX][newY] = res[x][y] + 1;
  //       visited[newX][newY] = true;
  //       q.push([newX, newY]);
  //     }
  //   }
  // }

  const res = [];
  const visited = [];
  for (let i = 0; i < m; i++) {
    res.push(new Array(n));
    visited.push(new Array(n).fill(false));
  }
  const q = [];
  // 如果(i,j)元素为0，则 距离为0
  for (let i = 0; i < m; i++) {
    for (let j = 0; j < n; j++) {
      if (matrix[i][j] === 0) {
        res[i][j] = 0;
        visited[i][j] = true;
        q.push([i, j]);
      }
    }
  }

  const d = [[0, -1], [0, 1], [1, 0], [-1, 0]];

  let level = 0;
  while (q.length) {
    let size = q.length;
    // 将同一层的东西全部处理完（每次处理的都是同一层）
    while (size--) {
      const [x, y] = q.shift();
      if (matrix[x][y] === 1) {
        res[x][y] = level;
      }
      for (const [offsetX, offsetY] of d) {
        const newX = x + offsetX;
        const newY = y + offsetY;
        // 没有访问过的地方一定是1，因为第一步中值为0的位置全部被标记了
        if (newX >= 0 && newX < m && newY >= 0 && newY < n && !visited[newX][newY]) {
          visited[newX][newY] = true;
          q.push([newX, newY]);
        }
      }
    }
    level++;
  }

  return res;
};

matrix = [
  [0, 0, 0],
  [0, 1, 0],
  [1, 1, 1],
];

ret = updateMatrix(matrix);
debugger;
