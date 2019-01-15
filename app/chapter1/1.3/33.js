// Deque。一个双向队列(或者称为deque)和栈或队列类似，但它同时支持在两端添加或删除元素。Deque能够存储一组元素并支持如下API。
// 使用动态数组和双线链表分别实现

const Node = require('../../DoubleNode');

class Deque {

    constructor() {
        this.head = null;
        this.tail = null;
        this.count = 0;
    }

    isEmpty() {
        return this.count === 0;
    }

    size() {
        return this.count;
    }

    pushLeft(value) {
        const node = new Node(value);
        node.next = this.head;
        if (this.head) {
            this.head.prev = node;
        } else {
            this.tail = node;
        }
        this.head = node;
        this.count++;
    }

    pushRight(value) {
        const node = new Node(value);
        node.prev = this.tail;
        if (this.tail) {
            this.tail.next = node;
        } else {
            this.head = node;
        }
        this.tail = node;
        this.count++;
    }

    popLeft() {
        const head = this.head;
        if (!head) return null;
        const next = head.next;
        if (next) {
            next.prev = null;
        }
        head.next = null;
        this.head = next;
        return head;
    }

    popRight() {
        const tail = this.tail;
        if (!tail) return null;
        const prev = tail.prev;
        if (prev) {
            prev.next = null;
        }
        tail.prev = null;
        this.tail = prev;
        this.count--;
    }
}

const q = new Deque();

q.pushLeft(1);
q.pushLeft(2);
q.pushLeft(3);
const d = q.popRight();
q.popLeft();
