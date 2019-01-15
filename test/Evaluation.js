'use strict'

class Evaluation {
  constructor() {
    this.opStack = [] // 操作符栈
    this.valueStack = [] // 操作数栈
  }

  isOperator(token) {
    return token == '+' || token == '-' || token == '*' || token == '/' || token == 'sqrt'
  }

  isLeftBracket(token) {
    return token == '('
  }

  isRightBracket(token) {
    return token == ')'
  }

  /**
   * 解析表达式字符串，得到tokens
   */
  parseExp(expStr) {
    return expStr.split(/\s+/g)
  }

  calculate(op, num1, num2) {
    if (op == '+') return num1 + num2
    if (op == '-') return num1 - num2
    if (op == '*') return num1 * num2
    if (op == '/') return num1 / num2
    if (op == 'sqrt') return Math.sqrt(num1)
  }

  /**
   * 表达式字符串，注意空格分隔
   * ( 1 + ( ( 2 + 3 ) * ( 4 * 5 ) ) )
   */
  eval(expStr) {
    var tokens = this.parseExp(expStr)
    for (let token of tokens) {
      if (this.isLeftBracket(token)) continue
      if (this.isOperator(token)) this.opStack.push(token)
      else if (this.isRightBracket(token)) {
        var op = this.opStack.pop()
        var v = this.valueStack.pop();
        switch (op) {
          case '+': v = this.valueStack.pop() + v; break;
          case '-': v = this.valueStack.pop() - v; break;
          case '*': v = this.valueStack.pop() * v; break;
          case '/': v = this.valueStack.pop() / v; break;
          case 'sqrt': v = Math.sqrt(v); break;
        }
        this.valueStack.push(v);
      } else this.valueStack.push(Number(token))
    }
    return this.valueStack.pop();
  }
  /**
   * 中序表达式转化为后序表达式
   * ( ( 1 + 2 ) * ( ( 3 - 4 ) * ( 5 - 6 ) ) )  将转化为1 2 + 3 4 - 5 6 - * * 
   */
  infixToPostfix(expStr) {
    // 1.忽略左括号
    // 2.遇到数字将其放入值栈
    // 3.遇到符号将其放入符号栈
    // 4.遇到右括号，计算后缀表达式，同时放入值栈
    var tokens = this.parseExp(expStr)
    for (let token of tokens) {
      if (this.isOperator(token)) this.opStack.push(token)
      else if (this.isLeftBracket(token)) continue
      else if (this.isRightBracket(token)) {
        var num2 = this.valueStack.pop()
        var num1 = this.valueStack.pop()
        var op = this.opStack.pop()
        this.valueStack.push(`${num1} ${num2} ${op}`)
      } else {
        this.valueStack.push(token)
      }
    }
    return this.valueStack.pop()
  }
  /**
   * 后序表达式求值,参考 #infixToPostfix
   * 后序表达式没有括号的问题，处理起来比较简单，使用一个栈即可
   * 1 2 + 3 4 - 5 6 - * * => ( ( 1 + 2 ) * ( ( 3 - 4 ) * ( 5 - 6 ) ) ) => 3
   */
  evaluatePostfix(postfixExp) {
    var tokens = this.parseExp(postfixExp)
    // 数字放值栈
    // 遇到符号出栈2个操作数，并将计算结果放入值栈
    // 计算到最后值栈中仅有一个值就是表达式的值
    for (let token of tokens) {
      if (this.isOperator(token)) {
        let num2 = this.valueStack.pop(),
          num1 = this.valueStack.pop()
        this.valueStack.push(this.calculate(token, num1, num2))
      } else {
        this.valueStack.push(Number(token))
      }
    }
    return this.valueStack.pop()
  }
}

var e = new Evaluation()
// var ret = e.eval('( 1 + ( ( 2 + 3 ) * ( 4 * 5 ) ) )') // number 101
// var ret = e.infixToPostfix('( ( 1 + 2 ) * ( ( 3 - 4 ) * ( 5 - 6 ) ) )') // 1 2 + 3 4 - 5 6 - * *
// var ret = e.evaluatePostfix('1 2 + 3 4 - 5 6 - * *') // number 3
// console.log(typeof ret,ret);