/**
 * 栈的链表实现
 * 
 * 表头插入、表头删除（针对表头操作只需要操作head指针即可）
 */

const Node = require('./Node');

class LinkedStack {

    constructor() {
        this.head = null;
        this.size = 0;
    }

    push(item) {
        this.head = new Node(item, this.head);
        this.size++;
    }

    pop() {
        if (this.isEmpty()) return null;
        const item = this.head.value;
        this.head = this.head.next;
        this.size--;
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

    peek() {
        if (this.isEmpty()) return null;
        return this.head.value;
    }

    static copy(stack) {
        // 2次遍历得到正确的顺序
        const tmp = new LinkedStack();
        const result = new LinkedStack();
        for (const item of stack) {
            tmp.push(item);
        }
        for (const item of tmp) {
            result.push(item);
        }
        return result;
    }

    /**
     * 连接另外一个栈
     */
    catenation(stack) {
        for (let item of stack) {
            this.push(item);
        }
    }

    toArray() {
        const ret = [];
        for (let item of this) {
            ret.push(item);
        }
        return ret;
    }
}

module.exports = LinkedStack;

// const stack = new LinkedStack();
// stack.push(1);
// stack.push(2);
// stack.push(3);
// stack.push(4);

// const stack2 = new LinkedStack();
// stack2.push(11);
// stack2.push(22);
// stack2.push(33);

// stack.catenation(stack2);
// debugger
