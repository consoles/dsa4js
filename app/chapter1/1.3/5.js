// 1.3.5 当N为50时下面这段代码会打印什么？从较高的抽象层次描述给定正整数N时这段代码的行为
// 二进制表示

const Stack = require('../../LinkedStack');

const stack = new Stack();

let n = 50;
while (n > 0) {
    stack.push(n % 2);
    n = parseInt(n / 2);
}

// 110010
for (let item of stack) {
    console.log(item);
}

const toBinaryStr = num => {
    let str = '';
    while (num > 0) {
        const mod = num % 2;
        num = parseInt(num / 2);
        str = mod + str;
    }
    return str;
};

const ret = toBinaryStr(50);
debugger