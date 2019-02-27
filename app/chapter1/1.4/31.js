// 三个栈实现的双向队列。使用三个栈实现一个双向队列，使得双向队列的每个操作所需的栈操作均摊之后为一个常数。

const Stack = require('../../LinkedStack');

class Deque {
    constructor() {
        this.leftStack = new Stack();
        this.rightStack = new Stack();
    }
    pushLeft(item) {
        this.leftStack.push(item);
    }
    pushRight(item) {
        this.rightStack.push(item);
    }
    _move(src, dest) {
        while (!src.isEmpty()) {
            dest.push(src.pop());
        }
    }
    popLeft() {
        if (this.leftStack.isEmpty()) {
            this._move(this.rightStack, this.leftStack);
        }
        return this.leftStack.pop();
    }
    popRight() {
        if (this.rightStack.isEmpty()) {
            this._move(this.leftStack, this.rightStack);
        }
        return this.rightStack.pop();
    }
}

class Deque2 extends Deque {
    constructor() {
        // 左侧栈和右侧栈负责模拟队列，和用两个栈模拟队列的方法类似。
        // 由于是双向队列，左栈和右栈会频繁的倒来倒去，因此每次都只倒一半的元素可以有效减少开销。
        // 有一侧栈为空时，另一侧栈中上半部分先移动到中间栈中，下半部分倒到另一侧栈里，再从中间栈拿回上半部分元素。
        // 这样可以确保接下来的 pop 操作一定是常数级别的。
        super();
        this.middleStack = new Stack();
    }
    _move(src, dest) {
        const n = src.size;
        // 上半部分移动到临时栈middle
        for (let i = 0; i < Math.floor(n / 2); i++) {
            this.middleStack.push(src.pop());
        }
        // 下半部分移动到另一侧栈中
        while (!src.isEmpty()) {
            dest.push(src.pop());
        }
        // 从middle中取回上半部分元素
        while (!this.middleStack.isEmpty()) {
            src.push(this.middleStack.pop());
        }
    }
    isEmpty() {
        return this.leftStack.isEmpty() && this.rightStack.isEmpty() && this.middleStack.isEmpty();
    }
}

const d = new Deque2();
d.pushLeft(1);
d.pushLeft(2);
d.pushRight(3);
d.pushLeft(4);
d.pushLeft(5);
d.pushRight(6);

let x = d.popLeft();
x = d.popLeft();
x = d.popLeft();
x = d.popRight();
x = d.popRight();
x = d.popLeft();