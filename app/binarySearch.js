// 二分查找

'use strict'

let binarySearch = (arr, element, fn = (a, b) => a - b) => {

	let start = 0,
		end = arr.length - 1
	while (start <= end) {
		let mid = (start + end) >> 1
		const ret = fn(arr[mid], element);
		if (ret === 0) return mid;
		if (ret > 0) end = mid - 1;
		else start = mid + 1;
	}
	return -1
}

/**
 * 二分查找的递归实现，分治算法的典型应用
 */
let binarySearchByRecursion = (arr, element, start, end) => {

	if (start <= end) {
		let mid = (start + end) >> 1
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