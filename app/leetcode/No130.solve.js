/**
 * @param {character[][]} board
 * @return {void} Do not return anything, modify board in-place instead.
 * 
 * 给定的矩阵中存在 3 种元素：
 * 1: 字母 X
 * 2：被字母 X 包围的字母 O
 * 3：没有被字母 X 包围的字母 O
 * 
 * 本题要求将所有被字母 X 包围的 O 转化为字母 X，难点是判断哪些字母 O 是被字母 X 包围的
 * 
 * 题目解释到：任何边界上的 O 都不会被填充为 X => 所有不被包含的字母 O 都直接或者间接和边界上的 O 相连通
 * 最简单的方式是使用一个标记表示 边界上的 O 和间接和边界上的 O 联通的区域
 * 
 * 除了边缘的O以及和边缘O连通的O是不需要变成X的，其他都要变成X
 * 问题转化为连通区域问题。
 */
var solve = function (board) {
    const m = board.length
    if (m <= 0) return board
    const n = board[0].length

    /**
     * 将O以及周边的O转化为A
     * @param {*} i 
     * @param {*} j 
     */
    function mark(i, j) {
        if (i < 0 || i >= m || j < 0 || j >= n || board[i][j] !== 'O') return
        board[i][j] = 'A'
        mark(i + 1, j)
        mark(i - 1, j)
        mark(i, j + 1)
        mark(i, j - 1)
    }

    for (let i = 0; i < m; i++) {
        for (let j = 0; j < n; j++) {
            if (i === 0 || i === m - 1 || j === 0 || j === n - 1) {
                mark(i, j)
            }
        }
    }

    for (let i = 0; i < m; i++) {
        for (let j = 0; j < n; j++) {
            if (board[i][j] === 'O') {
                board[i][j] = 'X'
            } else if (board[i][j] === 'A') {
                board[i][j] = 'O'
            }
        }
    }

    return board
};

var solve2 = function (board) {
    const m = board.length
    const n = board[0].length
    // 使用 BFS
    // 多起点的 BFS
    const q = []
    for (let i = 0; i < m; i++) {
        for (let j = 0; j < n; j++) {
            if (i === 0 || i === m - 1 || j === 0 || j === n - 1) {
                if (board[i][j] === 'O') {
                    board[i][j] = 'A'
                    q.push([i, j])
                }
            }
        }
    }
    const d = [
        [0, 1], // down
        [1, 0], // right
        [0, -1], // up
        [-1, 0], // left
    ]
    while (q.length) {
        const [i, j] = q.shift()
        for (const [xOffset, yOffset] of d) {
            const x = i + xOffset
            const y = j + yOffset
            if (x >= 0 && x < m && y >= 0 && y < n && board[x][y] === 'O') {
                q.push([x, y])
                board[x][y] = 'A'
            }
        }
    }
    for (let i = 0; i < m; i++) {
        for (let j = 0; j < n; j++) {
            const c = board[i][j]
            if (c === 'A') {
                board[i][j] = 'O'
            } else if (c === 'O') {
                board[i][j] = 'X'
            }
        }
    }
}

var solve3 = function (board) {
    // 使用单一起点多次 BFS
    const m = board.length
    const n = board[0].length

    function bfs(i, j) {
        const d = [
            [0, 1], // down
            [1, 0], // right
            [0, -1], // up
            [-1, 0], // left
        ]
        // 在 BFS 中，“发现即标记” 是黄金法则！
        // 注意：标记和入队是一个不可分割的操作：可以确保每个节点只处理一次，如果等到出队的时候再标记可能导致重复入队的情况
        const q = [[i, j]]
        board[i][j] = 'A'
        while (q.length) {
            const [x, y] = q.shift()
            for (const [xOffset, yOffset] of d) {
                const newX = x + xOffset
                const newY = y + yOffset
                if (newX >= 0 && newX < m && newY >= 0 && newY < n && board[newX][newY] === 'O') {
                    q.push([newX, newY])
                    board[newX][newY] = 'A'
                }
            }
        }
    }

    for (let i = 0; i < m; i++) {
        for (let j = 0; j < n; j++) {
            if (i === 0 || i === m - 1 || j === 0 || j === n - 1) {
                if (board[i][j] === 'O') {
                    bfs(i, j)
                }
            }
        }
    }
    for (let i = 0; i < m; i++) {
        for (let j = 0; j < n; j++) {
            const c = board[i][j]
            if (c === 'A') {
                board[i][j] = 'O'
            } else if (c === 'O') {
                board[i][j] = 'X'
            }
        }
    }
}

var solve4 = function (board) {
    // 非递归的 DFS
    const m = board.length
    const n = board[0].length
    for (let i = 0; i < m; i++) {
        for (let j = 0; j < n; j++) {
            if (i === 0 || i === m - 1 || j === 0 || j === n - 1) {
                if (board[i][j] === 'O') {
                    dfs(i, j)
                }
            }
        }
    }

    function dfs(i, j) {
        const stack = [[i, j]]
        board[i][j] = 'A'
        // 我们每次查看栈顶，但是并不弹出，直到这个位置所有方向都搜索不到的时候弹出栈顶
        while(stack.length) {
            const [x, y] = stack[stack.length - 1]
            // down
            if (y + 1 < n && board[x][y + 1] === 'O') {
                stack.push([x, y + 1])
                board[x][y + 1] = 'A'
                continue
            } 
            // right
            if (x + 1 < m && board[x + 1][y] === 'O') {
                stack.push([x + 1, y])
                board[x + 1][y] = 'A'
                continue
            }
            // up
            if (y - 1 >= 0 && board[x][y - 1] === 'O') {
                stack.push([x, y - 1])
                board[x][y - 1] = 'A'
                continue
            }
            // left
            if (x - 1 >= 0 && board[x - 1][y] === 'O') {
                stack.push([x - 1, y])
                board[x - 1][y] = 'A'
                continue
            }
            // 如果上下左右 4 个方向都搜索不到，则本次搜索结束，弹出栈
            stack.pop()
        }
    }

    for (let i = 0; i < m; i++) {
        for (let j = 0; j < n; j++) {
            if (board[i][j] === 'O') {
                board[i][j] = 'X'
            } else if (board[i][j] === 'A') {
                board[i][j] = 'O'
            }
        }
    }
}

const board = [["X", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O"], ["O", "X", "O", "O", "O", "O", "X", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "X", "X"], ["O", "O", "O", "O", "O", "O", "O", "O", "X", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "X"], ["O", "O", "X", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "X", "O"], ["O", "O", "O", "O", "O", "X", "O", "O", "O", "O", "X", "O", "O", "O", "O", "O", "X", "O", "O", "X"], ["X", "O", "O", "O", "X", "O", "O", "O", "O", "O", "X", "O", "X", "O", "X", "O", "X", "O", "X", "O"], ["O", "O", "O", "O", "X", "O", "O", "X", "O", "O", "O", "O", "O", "X", "O", "O", "X", "O", "O", "O"], ["X", "O", "O", "O", "X", "X", "X", "O", "X", "O", "O", "O", "O", "X", "X", "O", "X", "O", "O", "O"], ["O", "O", "O", "O", "O", "X", "X", "X", "X", "O", "O", "O", "O", "X", "O", "O", "X", "O", "O", "O"], ["X", "O", "O", "O", "O", "X", "O", "O", "O", "O", "O", "O", "X", "X", "O", "O", "X", "O", "O", "X"], ["O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "X", "O", "O", "X", "O", "O", "O", "X", "O", "X"], ["O", "O", "O", "O", "X", "O", "X", "O", "O", "X", "X", "O", "O", "O", "O", "O", "X", "O", "O", "O"], ["X", "X", "O", "O", "O", "O", "O", "X", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O"], ["O", "X", "O", "X", "O", "O", "O", "X", "O", "X", "O", "O", "O", "X", "O", "X", "O", "X", "O", "O"], ["O", "O", "X", "O", "O", "O", "O", "O", "O", "O", "X", "O", "O", "O", "O", "O", "X", "O", "X", "O"], ["X", "X", "O", "O", "O", "O", "O", "O", "O", "O", "X", "O", "X", "X", "O", "O", "O", "X", "O", "O"], ["O", "O", "X", "O", "O", "O", "O", "O", "O", "O", "X", "O", "O", "X", "O", "X", "O", "X", "O", "O"], ["O", "O", "O", "X", "O", "O", "O", "O", "O", "X", "X", "X", "O", "O", "X", "O", "O", "O", "X", "O"], ["O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O"], ["X", "O", "O", "O", "O", "X", "O", "O", "O", "X", "X", "O", "O", "X", "O", "X", "O", "X", "O", "O"]]

solve3(board)
console.log(board)
