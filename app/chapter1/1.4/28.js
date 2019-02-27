// 一个队列实现的栈。使用一个队列实现一个栈，使得每个栈操作所需的队列操作数量为线性级别。
// 提示：要删除一个元素：将队列中除了最后一个元素的其他元素出列再入列

const Queue = require('../../LinkedQueue');

class Stack {
    constructor() {
        this.queue = new Queue();
        this.size = 0;
    }
    push(item) {
        this.size++;
        this.queue.enqueue(item);
    }
    pop() {
        // 除了最后一个元素不入队，其他都入队
        for (let i = 0; i < this.size - 1; i++) {
            this.queue.enqueue(this.queue.dequeue());
        }
        this.size--;
        return this.queue.dequeue();
    }
    isEmpty() {
        return this.size === 0;
    }
}

const s = new Stack();
for (let i = 1; i <= 5; i++) {
    s.push(i);
}
while (!s.isEmpty()) {
    console.log(s.pop());
}