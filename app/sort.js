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

// 比较的次数为(n-1-0) + (n-1-1) + (n-1-2) + ... + (n-1-(n-1))
// n(n-1) - n(n-1) / 2 = n(n-1) / 2
let selectionSort = arr => {
	for (let i = 0; i < arr.length; i++) {
		let minIndex = i
		// 比较的次数为n-1-i次
		for (let j = i + 1; j < arr.length; j++)
			if (arr[j] < arr[minIndex])
				minIndex = j
		// swap函数调用了n次（0~n-1）		
		swap(arr, i, minIndex)
	}
	return arr
}

let insertSort = arr => {
	for (let i = 1; i < arr.length; i++)
		// insert a[i] into arr[i-1],arr[i-2],arr[i-3],...,arr[0]
		for (let j = i; j > 0 && arr[j] < arr[j - 1]; j--)
			swap(arr, j, j - 1)
	return arr
}

let shellSort = arr => {
	for (let gap = arr.length >> 1; gap > 0; gap >>= 1)
		for (let i = gap; i < arr.length; i++)
			for (let j = i; j >= gap && arr[j] < arr[j - gap]; j -= gap)
				swap(arr, j, j - gap)
	return arr
}

// 采用不同的递增序列
// 在最坏的情况下：比较次数和N^3/2成正比
// 希尔排序比插入排序快得多，数组越大，优势越大
let shellSort2 = arr => {
	const n = arr.length;
	let h = 1;
	while (h < n / 3) {
		h = 3 * h + 1; // 1,4,13,40...
	}
	while (h >= 1) {
		// 将数组变为h有序
		// 在进行排序的时候，如果h很大，我们就能将元素移动到很远的地方，为实现更小的h有序创建方便。
		for (let i = h; i < n; i++) {
			// 将arr[i]插入到arr[i-h]、arr[i-2h]、arr[i-3h]中
			for (let j = i; j >= h && arr[j] < arr[j - h]; j -= h) {
				swap(arr, j, j - h);
			}
		}
		h = parseInt(h / 3);
	}
}

/**
 * 参见：http://blog.jobbole.com/100349/
 * 分治法，复杂度Nlog(N)
 */
var sortCount = 0,
	mergeCount = 0
let mergeSort = arr => {

	var merge = (left, right) => {
		var result = [],
			i = 0,
			j = 0

		// 组内排序,和数组长度相关，最后一次需要扫描整个数组长度，时间复杂度为N
		while (i < left.length && j < right.length) {
			if (left[i] < right[j]) result.push(left[i++])
			else result.push(right[j++])
		}

		// 将剩余排不完的有序数组加入到结果数组的末端
		var ret = result
			.concat(left.slice(i))
			.concat(right.slice(j))

		// console.log('result - %d : %j',mergeCount++,ret)	
		return ret
	}

	if (arr.length === 1) return arr

	var pos = arr.length / 2 >> 0

	// 拆分,将N*1个元素拆分为1*N的数组，时间复杂度为log(N)
	var left = arr.slice(0, pos),
		right = arr.slice(pos)

	// console.log('left - %d : %j',sortCount,left)	
	// console.log('right - %d : %j',sortCount,right)
	sortCount++

	// 合并，同理将1*N的数组两两合并,最终合并为N*1的数组，时间复杂度为log(N)
	return merge(mergeSort(left), mergeSort(right))
}

/**
 * 将[start,mid],[mid+1,end]的数组归并，形成一个大的有序数组arr[start...end]
 */
const merge = (arr, start, mid, end) => {
	const aux = [];
	let i = start;
	let j = mid + 1;
	for (let k = start; k <= end; k++) {
		if (i > mid) {
			aux.push(arr[j++]);
		} else if (j > end) {
			aux.push(arr[i++]);
		} else if (arr[i] < arr[j]) {
			aux.push(arr[i++]);
		} else {
			aux.push(arr[j++]);
		}
	}
	for (let i = 0; i < aux.length; i++) {
		arr[start + i] = aux[i];
	}
}

/**
 * 自底向上的归并排序
 */
const mergeSortBottomUp = arr => {
	const n = arr.length;
	for (let sz = 1; sz < n; sz *= 2) {
		for (let start = 0; start < n - sz; start += 2 * sz) {
			// 归并 arr[start...start+2*sz)
			const left = start;
			const right = Math.min(start + 2 * sz - 1, n - 1);// 防止右侧越界
			const mid = start + sz - 1;
			merge(arr, left, mid, right);
		}
	}
};

exports.selectionSort = selectionSort
exports.insertSort = insertSort
exports.bubbleSort = bubbleSort
exports.shellSort = shellSort
exports.mergeSort = mergeSort
exports.shellSort2 = shellSort2

exports.sortCompare = (n, t, ...sortFns) => {

	const ret = {};

	for (const sortFn of sortFns) {
		let total = 0;
		for (let i = 0; i < t; i++) {
			const arr = [];
			for (let j = 0; j < n; j++) {
				arr.push(Math.random());
			}
			const start = Date.now();
			sortFn.call(null, arr);
			total += Date.now() - start;
		}
		ret[sortFn.name] = total / t;
	}

	return ret;
}
