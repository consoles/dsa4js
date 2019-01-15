// Steque。一个以栈为目标的队列（或称steque），是一种支持push、pop和enqueue操作的数据类型。为这种抽象数据类型定义一份API并给出一份基于链表的实现
// push和pop是对队列同一端进行操作，enqueue是对队列的另一端进行操作

const Node = require('../../Node');

class Steque {

    constructor() {
        this.head = null;
    }

    push(value) {
        this.head = new Node(value, this.head);
    }

    pop() {
        if (!this.head) return null;
        const item = this.head.value;
        this.head = this.head.next;
        return item;
    }

    [Symbol.iterator]() {
        let cur = this.head;
        return {
            next() {
                if (cur) {
                    const value = cur.value;
                    cur = cur.next;
                    return { done: false, value };
                }
                return { done: true };
            }
        };
    }

    enqueue(value) {
        let cur = this.head;
        const node = new Node(value);
        if (!cur) {
            this.head = node;
            return;
        }
        while (cur.next) {
            cur = cur.next;
        }
        cur.next = node;
    }

    catenation(steque) {
        for (let item of steque) {
            this.enqueue(item);
        }
    }
}

const s = new Steque();
s.push(0);
s.push(1);
s.push(2);
s.push(3);

const s2 = new Steque();
s2.enqueue(11);
s2.enqueue(22);
s2.enqueue(33);
s.catenation(s2);
debugger