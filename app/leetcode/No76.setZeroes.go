package main

import "fmt"

func setZeroes(matrix [][]int) {
	zeroRows := make([]int, 0)
	zeroCols := make([]int, 0)
	for i := 0; i < len(matrix); i++ {
		for j := 0; j < len(matrix[i]); j++ {
			if matrix[i][j] == 0 {
				zeroRows = append(zeroRows, i)
				zeroCols = append(zeroCols, j)
			}
		}
	}
	for _, i := range zeroRows {
		for j := 0; j < len(matrix[i]); j++ {
			matrix[i][j] = 0
		}
	}
	for _, j := range zeroCols {
		for i := 0; i < len(matrix); i++ {
			matrix[i][j] = 0
		}
	}
}

func main() {
	matrix := [][]int{
		{1, 1, 1},
		{1, 0, 1},
		{1, 1, 1},
	}
	setZeroes(matrix)
	fmt.Println(matrix)
}
