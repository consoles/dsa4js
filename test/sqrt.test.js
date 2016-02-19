const expect = require('chai').expect

const sqrt = require('../app/sqrt')

/**
 * 注意：js的toFixed方法的返回值是string，需要转化成数字类型
 */

describe('<牛顿法求平方根的测试>', () => {
	it('#9的平方根应该等于3', () => {
		expect(sqrt(9)).to.be.equal(3)
	})
	it('#3的平方根应该等于1.732', () => {
		expect(+sqrt(3).toFixed(3)).to.be.equal(1.732)
	})
	it('#2的平方根应该等于1.414', () => {
		expect(+sqrt(2).toFixed(3)).to.be.equal(1.414)
	})
	it('#4的平方根应该等于2', () => {
		expect(sqrt(4)).to.be.equal(2)
	})
})