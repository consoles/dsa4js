// 删除第k个元素，实现一个类并支持isEmpty，insert，delete
// 分别用数组和链表实现

const assert = require('assert');

class Queue {
    constructor() {
        this.data = [];
    }
    isEmpty() {
        return this.data.length === 0;
    }
    insert(value) {
        this.data.push(value);
    }
    delete(k) {
        let index = k - 1;
        assert(index >= 0 && index < this.data.length);
        const item = this.data[index];
        // 向前前移动
        for (let i = index; i < this.data.length - 1; i++) {
            this.data[i] = this.data[i + 1];
        }
        this.data.pop();
        return item;
    }
}

const Node = require('../../Node');
class Queue2 {
    constructor() {
        this.head = null;
    }
    isEmpty() {
        return this.head === null;
    }
    insert(value) {
        this.head = new Node(value, this.head);
    }
    delete(k) {
        assert(k >= 1);
        const dummyHead = {
            next: this.head
        };
        let cur = dummyHead;
        let count = 1;
        while (count < k && cur) {
            const next = cur.next;
            if (!next) return;
            cur = next;
            count++;
        }
        cur.next = cur.next.next;
        this.head = dummyHead.next;
    }
}

const q = new Queue2();
for (let i = 0; i < 5; i++) {
    q.insert(i);
}
q.delete(1);
q.delete(3);
q.delete(3);
debugger