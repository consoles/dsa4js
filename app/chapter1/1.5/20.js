// 动态生长。
// 使用链表或大小可变的数组实现加权 quick-union 算法，
// 去掉需要预先知道对象数量的限制。
// 为 API 添加一个新方法 newSite()，
// 它应该返回一个类型为 int 的标识符。

const assert = require('assert');

class Node {
    constructor(value, next) {
        this.value = value;
        this.next = next;
    }
}

class LinkedList {
    constructor() {
        this.head = null;
        this.size = 0;
    }
    insertAsHead(value) {
        this.head = new Node(value, this.head);
        this.size++;
    }
    insert(value, position) {
        assert(position >= 0 && position <= this.size);
        if (position === 0) {
            this.insertAsHead(value);
            return;
        }

        let cur = this.head;
        for (let i = 1; i < position; i++) {
            cur = cur.next;
        }
        const node = new Node(value, cur.next);
        cur.next = node;
        this.size++;
    }
    find(position) {
        assert(position >= 0 && position < this.size, `${position} ${this.size}`);
        let cur = this.head;
        for (let i = 0; i < position; i++) {
            cur = cur.next;
        }
        return cur.value;
    }
    modify(index, value) {
        assert(index >= 0 && index < this.size, `${index} ${this.size}`);
        let cur = this.head;
        for (let i = 0; i < index; i++) {
            cur = cur.next;
        }
        if (cur) {
            cur.value = value;
        }
    }
    delete(index) {
        assert(index >= 0 && index < this.size);
        if (index === 0) {
            const value = this.head.value;
            this.head = this.head.next;
            this.size--;
            return value;
        }
        let cur = this.head;
        for (let i = 1; i < index; i++) {
            cur = cur.next;
        }
        const value = cur.next.value;
        cur.next = cur.next.next;
        this.size--;
        return value;
    }

}

class LinkedListUF {
    constructor() {
        // 将parent数组和size数组用链表实现即可
        this.parent = new LinkedList();
        this.size = new LinkedList();
        this.count = 0; // 连通分量数目
    }
    newSite(p) {
        this.parent.insert(this.parent.size, this.parent.size);
        this.size.insert(1, this.size.size);
        this.count++;
        return this.parent.size - 1;
    }
    find(p) {
        assert(p >= 0 && p < this.size.size);
        while (p !== this.parent.find(p)) {
            p = this.parent.find(p);
        }
        return p;
    }
    connected(p, q) {
        return this.find(p) === this.find(q);
    }
    union(p, q) {
        const pRoot = this.find(p);
        const qRoot = this.find(q);
        if (pRoot === qRoot) return;

        if (this.size.find(pRoot) < this.size.find(qRoot)) {
            this.parent.modify(pRoot, qRoot);
            this.size.modify(qRoot, this.size.find(qRoot) + this.size.find(pRoot));
        } else {
            this.parent.modify(qRoot, pRoot);
            this.size.modify(pRoot, this.size.find(pRoot) + this.size.find(qRoot));
        }
        this.count--;
    }
}

const pairs = [
    [0, 1],
    [2, 3],
    [3, 4],
    [2, 4],
    [1, 2],
    [0, 3]
];

const uf = new LinkedListUF();

for (let i = 0; i < 5; i++) {
    uf.newSite(i);
}

for (const [p, q] of pairs) {
    if (uf.connected(p, q)) {
        console.log(`${p} - ${q} connected`);
    } else {
        uf.union(p, q);
        console.log(`union ${p} ${q}`);
    }
}