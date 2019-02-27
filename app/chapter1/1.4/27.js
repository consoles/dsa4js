// 两个栈实现的队列。用两个栈实现一个队列，使得每个队列操作所需要的堆栈操作均摊后为一个常数。提示：如果将所有元素压入栈中再弹出，它们的顺序就被颠倒了。如果再次重复这个过程，它们的顺序则会复原。

const Stack = require('../../LinkedStack');

class Queue {
    constructor() {
        this.leftStack = new Stack();
        this.rightStack = new Stack();
        this.size = 0;
    }
    isEmpty() {
        return this.size === 0;
    }
    enqueue(item) {
        this.size++;
        this.leftStack.push(item);
    }
    dequeue() {
        this.size--;
        while (!this.leftStack.isEmpty()) {
            this.rightStack.push(this.leftStack.pop());
        }
        return this.rightStack.pop();
    }
}

const arr = [1, 2, 3, 4, 5, 6, 7, 8, 9];
const q = new Queue();
for (const item of arr) {
    q.enqueue(item);
}
while (!q.isEmpty()) {
    console.log(q.dequeue());
}