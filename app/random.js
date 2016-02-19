// 随机数工具类

'use strict'

/**
 * 得到[a,b)之间的随机数
 */
module.exports.uniform = (a, b) => a + Math.random() * (b - a)
