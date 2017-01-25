'use strict'

class Evaluation{
    constructor(){
        this.ops = [] // 操作符栈
        this.vals = [] // 操作数栈
    }
    /**
     * 表达式字符串，注意空格分隔
     * ( 1 + ( ( 2 + 3 ) * ( 4 * 5 ) ) )
     */
    eval(expStr){
        var tokens = expStr.split(/\s+/g)
        for(let token of tokens) {
            switch (token) {
                case '(' : break; // ignore left bracket
                case '+':
                case '-':
                case '*':
                case '/':
                case 'sqrt':
                this.ops.push(token);
                break;
                case ')':{
                    var op = this.ops.pop()
                    var v = this.vals.pop();
                    switch (op) {
                        case '+':v = this.vals.pop() + v;break;
                        case '-':v = this.vals.pop() - v;break;
                        case '*':v = this.vals.pop() * v;break;
                        case '/':v = this.vals.pop() / v;break;
                        case 'sqrt':v = Math.sqrt(v);break;
                    }
                    this.vals.push(v);
                    break;
                }
                default: this.vals.push(Number(token));break;
            }
        }
        return this.vals.pop();
    }
}

var e = new Evaluation()
var ret = e.eval('( 1 + ( ( 2 + 3 ) * ( 4 * 5 ) ) )')
console.log(typeof ret,ret);