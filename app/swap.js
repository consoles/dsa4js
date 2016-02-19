'use strict'

let swap = (arr, index1, index2) => {
	let temp = arr[index1]
	arr[index1] = arr[index2]
	arr[index2] = temp
	return arr
}

module.exports = swap