// 1.3.49 栈与队列。用有限个栈实现一个队列，保证每个队列操作（在最坏情况下）都只需要常数次的栈操作。(非常难)

// 6个栈的方案没看懂，这里就不写代码了，有时间可以参考下面的文章
// https://www.cnblogs.com/ikesnowy/p/7157813.html

const Stack = require('../../LinkedStack');

// 使用2个栈模拟队列
class Queue1 {
    constructor() {
        this.enqueStack = new Stack();
        this.dequeStack = new Stack();
    }
    enqueue(item) {
        this.enqueStack.push(item);
    }
    dequeue() {
        if (this.dequeStack.isEmpty()) {
            while (!this.enqueStack.isEmpty()) {
                this.dequeStack.push(this.enqueStack.pop());
            }
        }
        if (this.dequeStack.isEmpty()) {
            return null;
        }
        return this.dequeStack.pop();
    }
}

let q = new Queue1();
q.enqueue(1);
q.enqueue(2);
q.enqueue(3);
q.enqueue(4);
let ret = q.dequeue();
q.enqueue(5);
ret = q.dequeue();
debugger