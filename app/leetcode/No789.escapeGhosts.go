package main

func abs(x int) int {
	if x >= 0 {
		return x
	}
	return -x
}

func manhattanDistance(point1, point2 []int) int {
	return abs(point1[0]-point2[0]) + abs(point1[1]-point2[1])
}

func escapeGhosts(ghosts [][]int, target []int) bool {
	// 如果一个鬼能阻拦玩家，必然不会比玩家更晚到达终点
	dis := manhattanDistance([]int{0, 0}, target)
	for _, ghost := range ghosts {
		d := manhattanDistance(ghost, target)
		if d <= dis {
			return false
		}
	}
	return true
}
