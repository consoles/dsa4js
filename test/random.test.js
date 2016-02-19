'use strict'

const expect = require('chai').expect

const uniform = require('../app/random').uniform

describe('<随机数测试>', () => {
	it('#uniform', () => {
		for (let i = 0; i < 10000; i++) {
			expect(uniform(3, 8)).to.be.within(3, 8)
			expect(uniform(0, 15.9)).to.be.within(0, 15.9)
			expect(uniform(-2.6, 8.8)).to.be.within(-2.6, 8.8)
		}
	})
})