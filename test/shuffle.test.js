'use strict'

const shuffle = require('../app/shuffle')

describe('<数组打乱测试>', () => {
	it('#shuffle', () => {
		for(let i = 0;i < 10;i++){
			let arr1 = [1, 2, 3, 4, 5, 6, 7]
			let arr2 = shuffle(arr1)
			console.log(arr2)
		}
	})
})