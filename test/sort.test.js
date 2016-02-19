const expect = require('chai').expect

const selectionSort = require('../app/selectionSort')
const bubbleSort = require('../app/bubbleSort')

describe('<排序测试>', () => {
	it('#1 选择排序', () => {
		// eql 与 equal，ref:http://chaijs.com/api/bdd/#.equal(value)
		expect(selectionSort([1, 2, 3, 5, 4])).to.be.eql([1, 2, 3, 4, 5])
		expect(selectionSort([2, 2, 1, -4, 7, 9])).to.be.eql([-4, 1, 2, 2, 7, 9])
		expect(selectionSort([6, 2, 4, 1, 5, 9])).to.be.eql([1, 2, 4, 5, 6, 9])
		expect(selectionSort([5, 4, 3, 2, 1])).to.be.eql([1, 2, 3, 4, 5])
		expect(selectionSort([0, 9, -111, 99.3, 78.56, -54.3])).to.be.eql([-111, -54.3, 0, 9, 78.56, 99.3])
	})
	it('#2 冒泡排序', () => {
		expect(bubbleSort([1, 2, 3, 5, 4])).to.be.eql([1, 2, 3, 4, 5])
		expect(bubbleSort([2, 2, 1, -4, 7, 9])).to.be.eql([-4, 1, 2, 2, 7, 9])
		expect(bubbleSort([6, 2, 4, 1, 5, 9])).to.be.eql([1, 2, 4, 5, 6, 9])
		expect(bubbleSort([5, 4, 3, 2, 1])).to.be.eql([1, 2, 3, 4, 5])
		expect(bubbleSort([0, 9, -111, 99.3, 78.56, -54.3])).to.be.eql([-111, -54.3, 0, 9, 78.56, 99.3])
	})
})
