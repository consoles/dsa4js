// 编写一段程序EvaluatePostfix，从标准输入中得到一个后序表达式，求值并打印结果。

const Stack = require('../../LinkedStack');

// 1 2 + 3 4 - 5 6 - * * => ((1 + 2) * ((3 - 4) * (5 - 6)))

// 思路类似双栈法，但是扫描字符串的时候遇到操作符的时候就需要计算结果了，可以省掉opStack
// 遇到数字压入valueStack
// 遇到操作符从valueStack中弹出2个元素，并进行相关操作后压入valueStack
// 后缀表达式的值就是栈顶的值

const evalPostfix = str => {

    const valueStack = new Stack();

    for (let s of str) {
        if (/\d/.test(s)) {
            valueStack.push(s);
        } else if (['+', '-', '*', '/'].includes(s)) {
            const num2 = valueStack.pop();
            const num1 = valueStack.pop();
            valueStack.push(eval(`${num1} ${s} ${num2}`));
        }
    }

    return valueStack.pop();
};

const ret = evalPostfix('1 2 + 3 4 - 5 6 - * *');
debugger