// 编写一个过滤器InfixToPostfix，将算术表达式由中序表达式转为后序表达式。

// 与算术表达式求值使用的算法一样，值栈和符号栈。扫描字符串
// 忽略左括号
// 遇到数字压入valueStack
// 遇到符号压入opStack
// 遇到右括号从valueStack中弹出2个操作数，从opStack中弹出一个操作符，计算后缀表达式压入valueStack
// 最后valueStack中的值就是后缀表达式

const Stack = require('../../LinkedStack');

const convert = str => {

    const opStack = new Stack();
    const valueStack = new Stack();

    for (let s of str) {
        if (/\d/.test(s)) {
            valueStack.push(s);
        } else if (')' === s) {
            const num2 = valueStack.pop();
            const num1 = valueStack.pop();
            const op = opStack.pop();
            const exp = `${num1} ${num2} ${op}`;
            valueStack.push(exp);
        } else if (['+', '-', '*', '/'].includes(s)) {
            opStack.push(s);
        }
    }

    return valueStack.pop();
};

const ret = convert('((1 + 2) * ((3 - 4) * (5 - 6)))');
debugger