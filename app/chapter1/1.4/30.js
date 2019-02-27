// 一个栈和一个 steque 实现的双向队列。使用一个栈和一个 steque 实现一个双向队列（请见练习 1.3.32），使得双向队列的每个操作所需的栈和 steque 操作均摊后为一个常数。

const Stack = require('../../LinkedStack');

class Steque {
    constructor() {
        this.leftStack = new Stack();
        this.rightStack = new Stack();
        this.size = 0;
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
        this.size++;
    }
    pop() {
        this._rightToLeft();
        this.size--;
        return this.leftStack.pop();
    }
    enqueue(item) {
        this._leftToRight();
        this.size++;
        return this.rightStack.push(item);
    }
    isEmpty() {
        return this.size === 0;
    }
}

// steque 作为队列的头部，stack 作为队列的尾部。
class Deque {
    constructor() {
        this.stack = new Stack();
        this.steque = new Steque();
    }
    _stackToSteque() {
        while (!this.stack.isEmpty()) {
            this.steque.pop(this.stack.pop());
        }
    }
    _stequeToStack() {
        while (!this.steque.isEmpty()) {
            this.stack.push(this.steque.pop());
        }
    }
    pushLeft(item) {
        this.steque.push(item);
    }
    pushRight(item) {
        if (this.stack.isEmpty()) {
            this.steque.enqueue(item);
        } else {
            this.stack.push(item);
        }
    }
    popLeft() {
        if (this.steque.isEmpty()) {
            this._stackToSteque();
        }
        return this.steque.pop();
    }
    popRight() {
        if (this.stack.isEmpty()) {
            this._stequeToStack();
        }
        return this.stack.pop();
    }
    isEmpty() {
        return this.stack.isEmpty() && this.steque.isEmpty();
    }
}

const d = new Deque();
d.pushLeft(1)
d.pushLeft(2);
d.pushRight(3);
d.pushRight(4);
let s = d.popLeft();
s = d.popRight();
debugger