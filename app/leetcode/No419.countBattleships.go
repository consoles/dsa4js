package main

func countBattleships(board [][]byte) int {
	// 遍历矩阵中的每个位置 (i,j) 如果满足 board[i][j] = 'X'，将以 (i,j) 为起点的所有位置设置为空位
	m, n := len(board), len(board[0])
	res := 0
	for i, row := range board {
		for j, ch := range row {
			if ch == 'X' {
				row[j] = '.'
				for k := j + 1; k < n && row[k] == 'X'; k++ {
					row[k] = '.'
				}
				for k := i + 1; k < m && board[k][j] == 'X'; k++ {
					board[k][j] = '.'
				}
				res++
			}
		}
	}
	return res
}

func countBattleships2(board [][]byte) int {
	// 战舰个数 = 战舰头部的个数
	// 战舰的头部，该位置的左侧和上侧都没有战舰
	res := 0
	for i, row := range board {
		for j, ch := range row {
			if ch == 'X' && (j == 0 || row[j-1] != 'X') && (i == 0 || board[i-1][j] != 'X') {
				res++
			}
		}
	}
	return res
}
