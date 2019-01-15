// 实现一个嵌套类DoubleNode用来构造双向链表，其中每个结点都含有一个指向前驱元素的引用和一个指向后续元素的引用（如果不存在则为null）。为以下任务实现若干静态方法：在头插入结点、在表尾插入结点、从表头删除结点、从表尾删除结点、在指定结点前插入新结点、在指定结点之后插入新结点、删除指定结点。

const Node = require('../../DoubleNode');

class DoubleList {

    constructor() {
        this.head = null;
        this.tail = null;
    }

    insertHead(value) {
        const node = new Node(value);
        const oldHead = this.head;
        node.next = this.head;

        if (oldHead) {
            oldHead.prev = node;
        } else {
            this.tail = node;
        }
        this.head = node;
    }

    insertTail(value) {
        const node = new Node(value);
        const oldTail = this.tail;
        node.prev = oldTail;
        if (oldTail) {
            oldTail.next = node;
        } else {
            this.head = node;
        }
        this.tail = node;
    }

    removeHead() {
        const head = this.head;
        if (!head) return;

        const second = head.next;
        head.next = null;
        if (second) {
            second.prev = null;
        }
        this.head = second;
    }

    removeTail() {
        const tail = this.tail;
        if (!tail) return;

        const beforeTail = tail.prev;
        if (beforeTail) {
            beforeTail.next = null;
        }
        this.tail = beforeTail;
    }

    insertBefore(node, value) {
        if (node === this.head) {
            this.insertHead(value);
            return;
        }
        const newNode = new Node(value);
        const prevNode = node.prev;

        prevNode.next = newNode;
        newNode.prev = prevNode;
        newNode.next = node;
        node.prev = newNode;
    }

    insertAfter(node, value) {
        if (node === this.tail) {
            this.insertTail(value);
            return;
        }

        const newNode = new Node(value);
        const nextNode = node.next;

        newNode.prev = node;
        node.next = newNode;
        newNode.next = nextNode;
        nextNode.prev = newNode;
    }

    delete(node) {
        const prev = node.prev;
        const next = node.next;

        prev.next = next;
        next.prev = prev;

        if (!prev) {
            this.head = next;
        }
        if (!next) {
            this.tail = prev;
        }

        node.prev = null;
        node.next = null;
    }

    find(value) {
        let cur = this.head;
        while (cur) {
            if (cur.value === value) return cur;
            cur = cur.next;
        }
        return null;
    }
}

const list = new DoubleList();

list.insertTail(1);
list.insertTail(2);
list.insertTail(3);
list.insertTail(4);
list.insertTail(5);
// list.insertHead(0);
// list.removeHead();
// list.removeTail();
// const node = list.find(3);
// list.insertBefore(node, -1);
// list.insertAfter(node, 222);
// list.delete(node);
debugger
