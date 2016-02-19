// 选择排序
'use strict'

const swap = require('./swap')

let selectionSort = arr => {
	for (let i = 0; i < arr.length - 1; i++) {
		for (let j = i + 1; j < arr.length; j++)
			if (arr[i] > arr[j])
				swap(arr, i, j)
		// console.log(arr);	
	}
	return arr
}

module.exports = selectionSort