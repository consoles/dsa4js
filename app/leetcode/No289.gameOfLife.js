// 289. 生命游戏

// 给定一个包含 m × n 个格子的面板，每一个格子都可以看成是一个细胞。每个细胞都具有一个初始状态：1 即为活细胞（live），或 0 即为死细胞（dead）。每个细胞与其八个相邻位置（水平，垂直，对角线）的细胞都遵循以下四条生存定律：
//
// 如果活细胞周围八个位置的活细胞数少于两个，则该位置活细胞死亡；
// 如果活细胞周围八个位置有两个或三个活细胞，则该位置活细胞仍然存活；
// 如果活细胞周围八个位置有超过三个活细胞，则该位置活细胞死亡；
// 如果死细胞周围正好有三个活细胞，则该位置死细胞复活；
// 根据当前状态，写一个函数来计算面板上所有细胞的下一个（一次更新后的）状态。下一个状态是通过将上述规则同时应用于当前状态下的每个细胞所形成的，其中细胞的出生和死亡是同时发生的。
//
//  
//
// 示例：
//
// 输入：
// [
//   [0,1,0],
//   [0,0,1],
//   [1,1,1],
//   [0,0,0]
// ]
// 输出：
// [
//   [0,0,0],
//   [1,0,1],
//   [0,1,1],
//   [0,1,0]
// ]
//
//
// 进阶：
//
// 你可以使用原地算法解决本题吗？请注意，面板上所有格子需要同时被更新：你不能先更新某些格子，然后使用它们的更新后的值再更新其他格子。
// 本题中，我们使用二维数组来表示面板。原则上，面板是无限的，但当活细胞侵占了面板边界时会造成问题。你将如何解决这些问题？
//
// 来源：力扣（LeetCode）
// 链接：https://leetcode-cn.com/problems/game-of-life
//   著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。

/**
 * @param {number[][]} board
 * @return {void} Do not return anything, modify board in-place instead.
 */
var gameOfLife = function (board) {
  // 复制一份
  const bb = [];
  for (let i = 0; i < board.length; i++) {
    bb.push(board[i].slice());
  }

  function checkRange(board, x, y) {
    return x >= 0 && x < board.length && y >= 0 && y < board[x].length;
  }

  function getNextState(board, x, y) {
    let liveCount = 0;
    let dieCount = 0;

    let state = board[x][y];

    for (let i = x - 1; i <= x + 1; i++) {
      for (let j = y - 1; j <= y + 1; j++) {
        if (checkRange(board, i, j)) {
          if (i === x && j === y) continue;
          if (board[i][j] === 1) {
            liveCount++;
          } else {
            dieCount++;
          }
        }
      }
    }

    if (state === 1) {
      if (liveCount < 2 || liveCount > 3) {
        state = 0;
      }
    } else {
      if (liveCount === 3) {
        state = 1;
      }
    }
    return state;
  }

  for (let i = 0; i < bb.length; i++) {
    for (let j = 0; j < bb[i].length; j++) {
      board[i][j] = getNextState(bb, i, j);
    }
  }
};

var gameOfLife2 = function (board) {

  function checkRange(board, x, y) {
    return x >= 0 && x < board.length && y >= 0 && y < board[x].length;
  }

  function getNextState(board, x, y) {
    let liveCount = 0;
    let dieCount = 0;

    let state = board[x][y];

    for (let i = x - 1; i <= x + 1; i++) {
      for (let j = y - 1; j <= y + 1; j++) {
        if (checkRange(board, i, j)) {
          if (i === x && j === y) continue;
          if (board[i][j] === 1  || board[i][j]  === -1) {
            liveCount++;
          } else {
            dieCount++;
          }
        }
      }
    }

    // 引入中间态，是在是妙

    // state定义:1活，0死，2死变活，-1活变死
    if (state === 1) {
      if (liveCount < 2 || liveCount > 3) {
        state = -1;
      }
    } else {
      if (liveCount === 3) {
        state = 2;
      }
    }
    return board[x][y] = state;
  }

  for (let i = 0; i < board.length; i++) {
    for (let j = 0; j < board[i].length; j++) {
      getNextState(board, i, j);
    }
  }

  // 再次遍历，去除中间态，改为最终态
  for (let i = 0; i < board.length; i++) {
    for (let j = 0; j < board[i].length; j++) {
      board[i][j] = board[i][j] > 0 ? 1 : 0;
    }
  }
};


const board = [
  [0, 1, 0],
  [0, 0, 1],
  [1, 1, 1],
  [0, 0, 0]
];

gameOfLife2(board);
console.log(board);
