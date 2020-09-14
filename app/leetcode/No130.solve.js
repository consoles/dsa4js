/**
 * @param {character[][]} board
 * @return {void} Do not return anything, modify board in-place instead.
 * 
 * 除了边缘的O以及和边缘O连通的O是不需要变成X的，其他都要变成X
 * 问题转化为连通区域问题。
 */
var solve = function(board) {
    const m = board.length
    if (m <= 0) return board
    const n = board[0].length
    
    /**
     * 将O以及周边的O转化为A
     * @param {*} i 
     * @param {*} j 
     */
    function mark(i,j) {
        if(i < 0 || i >= m || j < 0 || j >= n || board[i][j] !== 'O') return 
        board[i][j] = 'A'
        mark(i + 1,j)
        mark(i - 1,j)
        mark(i,j + 1)
        mark(i,j - 1)
    }

    for(let i = 0;i < m;i++) {
        for(let j = 0;j < n;j++) {
            if (i === 0 || i === m - 1 || j === 0 || j === n - 1) {
                mark(i,j)
            }
        }
    }

    for(let i = 0;i < m;i++) {
        for(let j = 0;j < n;j++) {
            if (board[i][j] === 'O') {
                board[i][j] = 'X'
            } else if (board[i][j] === 'A') {
                board[i][j] = 'O'
            }
        }
    }

    return board
};

const board = [
    ["O","O","O","O","X","X"],
    ["O","O","O","O","O","O"],
    ["O","X","O","X","O","O"],
    ["O","X","O","O","X","O"],
    ["O","X","O","X","O","O"],
    ["O","X","O","O","O","O"]
]

console.log(solve(board));