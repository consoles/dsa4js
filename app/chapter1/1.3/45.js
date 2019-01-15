// 1.3.45 栈的可生成性。假设我们的栈测试用例会进行一系列的入栈和出栈操作，序列中的整数 0, 1, ... , N - 1 （按此先后顺序排列）表示入栈操作，N个减号表示出栈操作。设计一个算法，判定给定的混合序列是否会使数组向下溢出（你使用的空间量与 N 无关，即不能用某种数据结构存储所有整数）。设计一个线性时间算法判定我们的测试用例能否产生某个给定的排列（这取决于出栈操作指令的出现位置）。

const solve1 = nums => {
    let cnt = 0;
    for (const num of nums) {
        cnt += num == '-' ? -1 : 1;
        if (cnt < 0) return false;
    }
    return true;
}

// 对于某个整数k，前k次出栈操作会在前k次入栈操作之前完成，否则栈不会向下移除。
// 如果某个排列可以产生，那么产生它的方式一定是唯一的：如果输出排列中的下一个整数在栈顶，则将它弹出，否则将其压入栈中。
const solve2 = nums => {
    const Stack = require('../../LinkedStack');
    const stack = new Stack();
    const ops = [];
    let n = 0;
    stack.push(n);
    ops.push(n);
    n++;
    for (let i = 0; i < nums.length; i++) {
        while (n < nums.length && stack.peek() != nums[i]) {
            stack.push(n);
            ops.push(n);
            n++;
        }
        if (stack.peek() != nums[i]) {
            return null;
        }
        stack.pop();
        ops.push('-');
    }
    return ops;
}

console.log(solve1([0, 1, '-', '-', 3, '-', '-']));
console.log(solve2([2, 5, 6, 7, 4, 8, 9, 3, 1, 0]));
console.log(solve2([4, 6, 8, 7, 5, 3, 2, 9, 0, 1]));