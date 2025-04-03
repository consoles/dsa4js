function exist(board: string[][], word: string): boolean {
    const m = board.length;
    const n = board[0].length;
    const visited = new Array(m);
    for (let i = 0; i < m; i++) {
        visited[i] = new Array(n).fill(false);
    }

    function dfs(i: number, j: number, index: number): boolean {
        if (index === word.length - 1) {
            return board[i][j] === word[index];
        }
        if (board[i][j] !== word[index]) return false;

        const dirs = [[0, 1], [1, 0], [0, -1], [-1, 0]];
        // 当前位置匹配成功，向四周扩散尝试下一个位置
        visited[i][j] = true;
        for (const [xOffset, yOffset] of dirs) {
            const x = i + xOffset;
            const y = j + yOffset;
            if (x >= 0 && x < m && y >= 0 && y < n && !visited[x][y]) {
                const flag = dfs(x, y, index + 1);
                if (flag) return true;
            }
        }
        visited[i][j] = false; // 回溯
        return false;
    }

    for (let i = 0; i < m; i++) {
        for (let j = 0; j < n; j++) {
            if (board[i][j] === word[0]) {  // 匹配首字母
                if (dfs(i, j, 0)) return true;
            }
        }
    }
    return false;
}
