// 1.3.48 双向队列与栈。用一个双向队列实现两个栈，保证每个栈操作只需要常数次的双向队列操作。（请见练习 1.3.33)

const Node = require('../../DoubleNode');
class DeStack {
    constructor() {
        this.head = null;
        this.tail = null;
    }
    /**
     * 向左栈添加元素 
     */
    pushLeft(item) {
        const node = new Node(item);
        const oldHead = node.next = this.head;
        this.head = node;
        if (oldHead) {
            oldHead.prev = node;
        } else {
            this.tail = node;
        }
    }
    /**
     * 向右栈添加元素
     */
    pushRight(item) {
        const node = new Node(item);
        const oldTail = node.prev = this.tail;
        if (oldTail) {
            oldTail.next = node;
        } else {
            this.head = node;
        }
    }
    popLeft() {
        if (!this.head) return null;
        const item = this.head.value;
        this.head = this.head.next;
        return item;
    }
    popRight() {
        if (!this.tail) return null;
        const item = this.tail.value;
        this.tail = this.tail.prev;
        return item;
    }
}
