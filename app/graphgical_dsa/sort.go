package main

import "fmt"

func bubbleSort(arr []int) []int {
	notSortedIndex := len(arr) - 1
	for notSortedIndex > 0 {
		for i := 0; i < notSortedIndex; i++ {
			if arr[i] > arr[i+1] {
				arr[i], arr[i+1] = arr[i+1], arr[i]
			}
		}
		notSortedIndex--
	}
	return arr
}

func insertionSort(arr []int) []int {
	for i := 1; i < len(arr); i++ {
		for j := i; j > 0; j-- {
			if arr[j] < arr[j-1] {
				arr[j], arr[j-1] = arr[j-1], arr[j]
			}
		}
	}
	return arr
}

func selectionSort(arr []int) []int {
	for i := 0; i < len(arr); i++ {
		min := i
		for j := i + 1; j < len(arr); j++ {
			if arr[j] < arr[min] {
				min = j
			}
		}
		arr[i], arr[min] = arr[min], arr[i]
	}
	return arr
}

func _mergeSort(arr []int, lo int, hi int) {
	mid := lo + (hi-lo)/2
	_mergeSort(arr, lo, mid)
	_mergeSort(arr, mid+1, hi)
	_merge(arr, lo, mid, hi)
}

func _merge(arr []int, lo int, mid int, hi int) {
	if lo >= hi {
		return
	}
	// arr[lo...mid] and arr[mid+1...hi] are sorted => arr[lo...hi] make up a sorted array
	aux := make([]int, hi-lo+1)
	i := lo
	j := mid + 1
	for k := lo; k <= hi; k++ {
		if i > mid {
			aux[k-lo] = arr[j]
			j++
		} else if j > hi {
			aux[k-lo] = arr[i]
			i++
		} else if arr[i] < arr[j] {
			aux[k-lo] = arr[i]
			i++
		} else {
			aux[k-lo] = arr[j]
			j++
		}
	}
	for k := lo; k <= hi; k++ {
		arr[k] = aux[k-lo]
	}
}

func partition(arr []int) int {
	// 选择第一个元素作为基准
	v := arr[0]
	i := 0

	for j := 0; j < len(arr); j++ {
		if arr[j] < v {
			i++
			arr[i], arr[j] = arr[j], arr[i]
		}
	}

	arr[i], arr[len(arr)-1] = arr[len(arr)-1], arr[i]
	return i
}

func quickSort(arr []int) {
	if len(arr) <= 1 {
		return
	}
	p := partition(arr)
	quickSort(arr[:p])
	quickSort(arr[p+1:])
}

func mergeSort(arr []int) []int {
	_mergeSort(arr, 0, len(arr)-1)
	return arr
}

func main() {
	arr := []int{65, 55, 45, 35, 25, 15, 10}
	// insertionSort(arr)
	quickSort(arr)
	fmt.Printf("%v", arr)
}
