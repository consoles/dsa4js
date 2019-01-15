// 编写一段程序，从标准输入得到一个缺少左括号的表达式并打印出补全括号之后的中序表达式。例如，给定输入：
// 1 + 2 ) * 3 - 4 ) * 5 - 6 ) ) )
// 你的程序应该输出：
// ((1 + 2) * ((3 - 4) * (5 - 6)))

// 使用两个栈分别保存数值和操作符，分别为opStack和valueStack。顺序处理输入字符串的字符：
// 如果为操作符，压入opStack。
// 否则，如果为右括号，从valueStack取出两个操作数，从opStack取出1个操作符，添加括号组合后压入valueStack。
// 否则，为数字，压入valueStack。
// 以上处理办法需要输出满足以下条件，也就是有如下的假设：
// 输入表达式是合法的。

const Stack = require('../../LinkedStack');

const solve = str => {

    const opStack = new Stack();
    const valueStack = new Stack();

    for (let s of str) {
        if (['+', '-', '*', '/'].includes(s)) {
            opStack.push(s);
        } else if (s === ')') {
            const op = opStack.pop();
            const num2 = valueStack.pop();
            const num1 = valueStack.pop();
            const exp = `( ${num1} ${op} ${num2} )`;
            valueStack.push(exp);
        } else if (/\d/.test(s)) {
            valueStack.push(s);
        }
    }

    return valueStack.pop();
};

const ret = solve('1 + 2 ) * 3 - 4 ) * 5 - 6 ) ) )');
debugger