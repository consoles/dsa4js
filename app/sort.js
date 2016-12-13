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

/**
 * 参见：http://blog.jobbole.com/100349/
 * 分治法，复杂度Nlog(N)
 */
var sortCount = 0,
	mergeCount = 0
let mergeSort = arr => {

	var merge = (left,right) => {
		var result = [],
			i = 0,
			j = 0

		// 组内排序,和数组长度相关，最后一次需要扫描整个数组长度，时间复杂度为N
		while(i < left.length && j < right.length) {
			if (left[i] < right[j]) result.push(left[i++])
			else result.push(right[j++])
		}
		
		// 将剩余排不完的有序数组加入到结果数组的末端
		var ret = result
			.concat(left.slice(i))
			.concat(right.slice(j))

		console.log('result - %d : %j',mergeCount++,ret)	
		return ret	
	}

	if (arr.length === 1) return arr

	var pos = arr.length / 2 >> 0 
	
	// 拆分,将N*1个元素拆分为1*N的数组，时间复杂度为log(N)
	var left = arr.slice(0,pos),
		right = arr.slice(pos)

	console.log('left - %d : %j',sortCount,left)	
	console.log('right - %d : %j',sortCount,right)
	sortCount++	

	// 合并，同理将1*N的数组两两合并,最终合并为N*1的数组，时间复杂度为log(N)
	return merge(mergeSort(left),mergeSort(right))	
}

exports.selectionSort = selectionSort
exports.insertSort = insertSort
exports.bubbleSort = bubbleSort
exports.shellSort = shellSort
exports.mergeSort = mergeSort
