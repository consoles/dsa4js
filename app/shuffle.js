// 将数组中的元素随机打乱顺序
'use strict'

const uniform = require('./random').uniform
const swap = require('./swap');

/**
 * 从数组中删除指定的元素
 */
let genArr = (arr, index) => {

	let tmpArr = []
	for (let i = 0; i < arr.length; i++) {
		if (i === index)
			continue
		tmpArr.push(arr[i])
	}
	return tmpArr
}

let shuffle = arr => {
	for (let i = 0; i < arr.length; i++) {
		// 将arr[i]与和除自身之外的任意值交换，其中待交换的区间是[0,i)∪[i+1,arr.length)
		let tmpArr = genArr(arr, i)
		let randomIndex = uniform(0, tmpArr.length)
		swap(arr, i, parseInt(randomIndex))
	}
	return arr
}

module.exports = shuffle