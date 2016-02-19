// 冒泡排序
'use strict'

const swap = require('./swap');

let bubbleSort = arr => {
	for (let i = 0; i < arr.length; i++) {
		for (let j = 0; j < arr.length - 1; j++)
			if (arr[j + 1] < arr[j])
				swap(arr, j, j + 1)
		// console.log(arr)
	}
	return arr
}

module.exports = bubbleSort
