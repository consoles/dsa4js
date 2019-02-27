// 两个栈实现的 steque。用两个栈实现一个 steque（请见练习 1.3.32）,使得每个 steque 操作所需的栈操作均摊后为一个常数。

const Stack = require('../../LinkedStack');

// 任意一个操作，都保持一个栈为空，并保证其顺序
class Steque {
    constructor() {
        this.leftStack = new Stack();
        this.rightStack = new Stack();
    }
    _rightToLeft() {
        while (!this.rightStack.isEmpty()) {
            this.leftStack.push(this.rightStack.pop());
        }
    }
    _leftToRight() {
        while (!this.leftStack.isEmpty()) {
            this.rightStack.push(this.leftStack.pop());
        }
    }
    push(item) {
        this._rightToLeft();
        this.leftStack.push(item);
    }
    pop() {
        this._rightToLeft();
        return this.leftStack.pop();
    }
    enqueue(item) {
        this._leftToRight();
        return this.rightStack.push(item);
    }
}

const s = new Steque();
s.push(1);
s.push(2);
s.push(3);
s.enqueue(4);
s.enqueue(5);
s.push(6);
debugger
