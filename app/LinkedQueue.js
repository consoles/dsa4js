const Node = require('./Node');

/**
 * 基于链表的队列
 * 
 * 入队：表尾插入
 * 出队：表头删除（为什么出队操作使用表头删除？表头删除非常简单，表尾删除需要遍历）
 */
class LinkedQueue {

    constructor() {
        this.head = null;
        this.tail = null;
        this.size = 0;
    }

    copy(q) {
        const ret = new LinkedQueue();
        for (let item of q) {
            ret.enqueue(item);
        }
        return ret;
    }

    getHead(){
      return this.head.value;
    }

    enqueue(item) {

        const node = new Node(item);

        if (this.isEmpty()) {
            this.head = this.tail = node;
        } else {
            this.tail.next = node;
            this.tail = node;
        }
        this.size++;
    }

    dequeue() {
        if (this.isEmpty()) {
            return null;
        }
        this.size--;
        const item = this.head.value;
        const curHead = this.head = this.head.next;
        if (!curHead) {
            this.tail = null;
        }
        return item;
    }

    isEmpty() {
        return this.size === 0;
    }

    [Symbol.iterator]() {
        let cur = this.head;
        return {
            next() {
                if (cur) {
                    let value = cur.value;
                    cur = cur.next;
                    return { done: false, value };
                }
                return { done: true };
            }
        };
    }

    catenation(queue) {
        for (const item of queue) {
            this.enqueue(item);
        }
    }
}

module.exports = LinkedQueue;

// const q = new LinkedQueue();
// q.enqueue(1);
// q.enqueue(2);
// q.enqueue(3);

// const q2 = new LinkedQueue();
// q2.enqueue(11);
// q2.enqueue(22);
// q2.enqueue(33);
// q.catenation(q2);
// debugger
