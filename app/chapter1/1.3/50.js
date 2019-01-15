// 修改Stack的迭代器代码，确保一旦用例在迭代器中(通过push()或pop()操作）修改集合数据就抛出一个java.util.ConcurrentModificationException异
// 解答：用一个计数器记录push()和pop()操作的次数。在创建迭代器时，将该值记录到Iterator的一个实例变量中。在每次调用hasNext()和next()之前，检查该值是否发生了变化，如果变化则抛出异常。

const Node = require('../../Node');

class Stack {

    constructor() {
        this.head = null;
        this.size = 0;
        this.modCount = 0;
    }

    push(item) {
        this.head = new Node(item, this.head);
        this.size++;
        this.modCount++;
    }

    pop() {
        if (this.isEmpty()) return null;
        const item = this.head.value;
        this.head = this.head.next;
        this.size--;
        this.modCount++;
        return item;
    }

    isEmpty() {
        return this.size === 0;
    }

    [Symbol.iterator]() {
        let cur = this.head;
        const mCount = this.modCount;
        const self = this;
        return {
            next() {
                if (mCount !== self.modCount) throw new Error('在迭代的时候不能修改元素');
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
}

const s = new Stack();
s.push(1);
s.push(2);
s.push(3);
s.push(4);

for (const item of s) {
    console.log(item);
    s.push(6);
}
