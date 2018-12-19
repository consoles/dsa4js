// 编写一个递归方法求解ln(N!)

const math = require('mathjs');

const lnn = n => n === 1 ? 0 : Math.log2(n) + lnn(n - 1);

const ret = lnn(10);
const ret2 = Math.log2(math.factorial(10));
debugger