'use strict'

const swap = require('./swap')

let bubbleSort = arr => {
	for (let i = 0; i < arr.length; i++) {
		for (let j = 0; j < arr.length - 1; j++)
			if (arr[j + 1] < arr[j])
				swap(arr, j, j + 1)
				// console.log(arr)
	}
	return arr
}

let selectionSort = arr => {
	for (let i = 0; i < arr.length; i++) {
		let minIndex = i
		for (let j = i + 1; j < arr.length; j++)
			if (arr[j] < arr[minIndex])
				minIndex = j
		swap(arr, i, minIndex)
	}
	return arr
}

let insertSort = arr => {
	for (let i = 0; i < arr.length; i++)
		// insert a[i] into arr[i-1],arr[i-2],arr[i-3],...,arr[0]
		for (let j = i; j > 0 && arr[j] < arr[j - 1]; j--)
			swap(arr, j, j - 1)
	return arr
}

let shellSort = arr => {
	for(let gap = arr.length >> 1;gap > 0;gap >>= 1)
		for(let i = gap;i < arr.length;i++)
			for (let j = i; j >= gap && arr[j] < arr[j - gap]; j-= gap)
				swap(arr, j, j - gap)
	return arr		
}

exports.selectionSort = selectionSort
exports.insertSort = insertSort
exports.bubbleSort = bubbleSort
exports.shellSort = shellSort
