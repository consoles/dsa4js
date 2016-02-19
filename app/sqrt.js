// 牛顿迭代法求平方根，ref:http://www.blogjava.net/tinysun/archive/2009/05/01/242126.html

'use strict'

let sqrt = (x) => {

	const ERR = 1e-15

	if (x < 0)
		return NaN

	let x1 = x / 2 // 初始值可以取任意数，如果取神奇数0x5f37642f会明显提升速度
	let x2 = 0.0
	let err = x2 - x1

	while (Math.abs(err) > ERR) {
		x2 = x1 - (x1 * x1 - x) / (2 * x1)
		err = x2 - x1
		x1 = x2
	}
	return x2
}

module.exports = sqrt