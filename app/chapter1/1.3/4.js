// 编写一个Stack的用例Parentheses，从标准输入读取一个文本流并使用栈判定其中的括号是否配对完整。例如，对于[()]{ } { [()()]() } 程序应该打印true，对于[(]) 则打印false。

const Stack = require('../../LinkedStack');

const SYMBOL = {
    LEFT: ['(', '[', '{'],
    RIGHT: [')', ']', '}']
};

const MATCH = {
    ')': '(',
    ']': '[',
    '}': '{'
};

const check = str => {

    const stack = new Stack();

    for (let s of str) {
        if (SYMBOL.LEFT.includes(s)) {
            stack.push(s);
        } else if (SYMBOL.RIGHT.includes(s)) {
            let top = stack.pop();
            if (MATCH[s] !== top) {
                return false;
            }
        }
    }

    return stack.isEmpty();
};

let flag = check('[()]{ } { [()()]() }');
flag = check('[(])');
debugger