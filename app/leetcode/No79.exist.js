/**
 * @param {character[][]} board
 * @param {string} word
 * @return {boolean}
 */
var exist = function(board, word) {

    const m = board.length
    const n = board[0].length

    if (m === 0) {
        return false
    }

    const visited = new Array(board.length)
    

    for(let i = 0;i < m;i++) {
        visited[i] = new Array(n).fill(false)
    }
    function canGo(i,j) {
        return i >= 0 && i < m && j >= 0 && j < n
    }

    const dirs = [[0,1],[1,0],[0,-1],[-1,0]]

    function dfs(i,j,start) {
        if (start === word.length - 1) return board[i][j] === word[word.length - 1]
        
        if (board[i][j] === word[start]) {
            visited[i][j] = true
            for(const [offsetX,offsetY] of dirs) {
                const x = i + offsetX
                const y = j + offsetY
                if (canGo(x,y) && !visited[x][y]) {
                    const flag = dfs(x,y,start + 1)    
                    if (flag) {
                        return flag
                    }
                }
            }
            visited[i][j] = false
        }
       
        return false
    }

    for(let i = 0;i < m;i++) {
        for(let j = 0;j < n;j++) {
            // 剪枝
            if (board[i][j] === word[0]) {
                const flag = dfs(i,j,0)
                if (flag) {
                    return flag
                }
            }
        }
    }
    return false
};

board =
[
  ['A','B','C','E'],
  ['S','F','C','S'],
  ['A','D','E','E']
]

console.log(exist(board,'ABCCED'));
console.log(exist(board,'SEE'));
console.log(exist(board,'ABCB'));

board = [["F","Y","C","E","N","R","D"],["K","L","N","F","I","N","U"],["A","A","A","R","A","H","R"],["N","D","K","L","P","N","E"],["A","L","A","N","S","A","P"],["O","O","G","O","T","P","N"],["H","P","O","L","A","N","O"]],
word = "poland";

console.log(exist(board,word));