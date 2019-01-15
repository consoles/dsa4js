// 用环形链表实现Queue。环形链表也是一条链表，只是没有任何结点链接为空，且只要链表非空则last.next的值就为first。只能使用一个Node类型的实例变量（last）。

const Node = require('../../Node');

class Queue {
    constructor() {
        this.last = null;
    }
    enqueue(item) {
        const node = new Node(item);
        if (this.last == null) {
            this.last = node;
            node.next = node;
        } else {
            node.next = this.last.next;
            this.last.next = node;
            this.last = node;
        }
    }
    dequeue() {
        if (this.isEmpty()) return null;
        const item = this.last.next.value;
        if (this.last.next == this.last) {
            this.last = null;
        } else {
            this.last.next = this.last.next.next;
        }
        return item;
    }
    isEmpty() {
        return this.last === null;
    }
}

const q = new Queue();
for (let i = 1; i < 5; i++) {
    q.enqueue(i);
}
while (!q.isEmpty()) {
    const item = q.dequeue();
    console.log('dequeue', item);
}