'use strict'

const expect = require('chai').expect

const binarySearch = require('../app/binarySearch').binarySearch
const binarySearchByRecursion = require('../app/binarySearch').binarySearchByRecursion

describe('<二分查找测试>', () => {
	it('#binarySearch', () => {
		let arr = [1,2,3,4,5,6,7,8]
		expect(binarySearch(arr,1)).to.be.equal(0)
		expect(binarySearch(arr,4)).to.be.equal(3)
		expect(binarySearch(arr,7)).to.be.equal(6)
		expect(binarySearch(arr,8)).to.be.equal(7)
		expect(binarySearch(arr,99)).to.be.equal(-1)
	})
	it('#binarySearchByRecursion', () => {
		let arr = [1,2,3,4,5,6,7,8]
		let start = 0,
			end = arr.length - 1
		expect(binarySearchByRecursion(arr,1,start,end)).to.be.equal(0)
		expect(binarySearchByRecursion(arr,4,start,end)).to.be.equal(3)
		expect(binarySearchByRecursion(arr,7,start,end)).to.be.equal(6)
		expect(binarySearchByRecursion(arr,8,start,end)).to.be.equal(7)
		expect(binarySearchByRecursion(arr,99,start,end)).to.be.equal(-1)
	})
})
