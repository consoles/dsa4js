package main

import "fmt"

func maxAreaOfIsland(grid [][]int) int {
	visited := make([][]bool, len(grid))
	for i := 0; i < len(grid); i++ {
		visited[i] = make([]bool, len(grid[0]))
	}
	var maxCount = 0
	for i := 0; i < len(grid); i++ {
		for j := 0; j < len(grid[i]); j++ {
			if visited[i][j] {
				continue
			}
			if grid[i][j] == 1 {
				c := dfs(i, j, grid, visited)
				if c > maxCount {
					maxCount = c
				}
			}
		}
	}
	return maxCount
}

func dfs(i, j int, grid [][]int, visited [][]bool) int {
	visited[i][j] = true
	// 以 (i,j) 为起点，向上下左右 4 个方向寻找连续的 1 填充 visited
	dirs := [4][2]int{
		{-1, 0}, // up
		{0, 1},  // right
		{1, 0},  // down
		{0, -1},
	}
	count := 1
	for _, dir := range dirs {
		x := i + dir[0]
		y := j + dir[1]
		if x >= 0 && x < len(grid) && y >= 0 && y < len(grid[0]) && grid[x][y] == 1 && !visited[x][y] {
			c := dfs(x, y, grid, visited)
			count += c
		}
	}
	return count
}

func dfs2(i, j int, grid [][]int) int {
	if i < 0 || i >= len(grid) || j < 0 || j >= len(grid[i]) || grid[i][j] == 0 {
		return 0
	}
	grid[i][j] = 0
	num := 1
	num += dfs2(i+1, j, grid)
	num += dfs2(i-1, j, grid)
	num += dfs2(i, j+1, grid)
	num += dfs2(i, j-1, grid)
	return num
}

func maxAreaOfIsland2(grid [][]int) int {
	// 沉岛思想：每次找到岛屿，直接将该岛屿改成 0，原地修改 grid
	maxCount := 0
	for i := 0; i < len(grid); i++ {
		for j := 0; j < len(grid[i]); j++ {
			if grid[i][j] == 1 {
				c := dfs2(i, j, grid)
				if c > maxCount {
					maxCount = c
				}
			}
		}
	}
	return maxCount
}

func maxAreaOfIsland3(grid [][]int) int {
	m := len(grid)
	n := len(grid[0])

	// 使用 栈 模拟 dfs
	maxCount := 0
	for i := 0; i < m; i++ {
		for j := 0; j < n; j++ {
			if grid[i][j] == 1 {
				stack := make([][]int, 0)
				stack = append(stack, []int{i, j})
				s := 0
				for len(stack) > 0 {
					pos := stack[len(stack)-1]
					stack = stack[:len(stack)-1]
					x := pos[0]
					y := pos[1]
					if x >= 0 && x < m && y >= 0 && y < n && grid[x][y] == 1 {
						s++
						grid[x][y] = 0
						stack = append(stack, []int{x + 1, y})
						stack = append(stack, []int{x - 1, y})
						stack = append(stack, []int{x, y + 1})
						stack = append(stack, []int{x, y - 1})
					}
				}
				if s > maxCount {
					maxCount = s
				}
			}
		}
	}
	return maxCount
}

func maxAreaOfIsland4(grid [][]int) int {
	m := len(grid)
	n := len(grid[0])
	maxCount := 0

	// bfs
	for i := 0; i < m; i++ {
		for j := 0; j < n; j++ {
			if grid[i][j] == 0 {
				continue
			}
			s := 0
			q := make([][]int, 0)
			q = append(q, []int{i, j})
			for len(q) > 0 {
				pos := q[0]
				q = q[1:]
				x := pos[0]
				y := pos[1]
				if x >= 0 && x < m && y >= 0 && y < n && grid[x][y] == 1 {
					s++
					grid[x][y] = 1
					q = append(q, []int{x + 1, y})
					q = append(q, []int{x - 1, y})
					q = append(q, []int{x, y + 1})
					q = append(q, []int{x, y - 1})
				}
			}
			if s > maxCount {
				maxCount = s
			}
		}
	}

	return maxCount
}

func main() {
	// grid := [][]int{{0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0}, {0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0}, {0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0}}
	// r := maxAreaOfIsland(grid) // 6

	grid := [][]int{{1, 1, 0, 0, 0}, {1, 1, 0, 0, 0}, {0, 0, 0, 1, 1}, {0, 0, 0, 1, 1}}
	r := maxAreaOfIsland2(grid)
	fmt.Println(r)
}
