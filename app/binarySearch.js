// 二分查找

'use strict'

let binarySearch = (arr, element) => {

	let start = 0,
		end = arr.length - 1
	while (start <= end) {
		let mid = parseInt((start + end) / 2)
		if (element === arr[mid])
			return mid
		if (element > arr[mid])
			start = mid + 1
		else
			end = mid - 1
	}
	return -1
}

/**
 * 二分查找的递归实现，分治算法的典型应用
 */
let binarySearchByRecursion = (arr, element, start, end) => {

	if (start <= end) {
		let mid = parseInt((start + end) / 2)
		if (arr[mid] === element)
			return mid
		if (element > arr[mid])
			return binarySearchByRecursion(arr, element, mid + 1, end)
		return binarySearchByRecursion(arr, element, start, end - 1)
	}
	return -1
}

exports.binarySearch = binarySearch
exports.binarySearchByRecursion = binarySearchByRecursion