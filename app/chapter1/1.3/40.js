// 1.3.40 前移编码。从标准输入读取一串字符，使用链表保存这些字符并删除重复字符。当你读取了一个从未见过的字符时，将它插入表头。当你读取了一个重复的字符时，将它从链表中删去并再次插入表头。将你的程序命名为 MoveToFront：它实现了著名的前移编码策略，这种策略假设最近访问过的元素很可能会再次访问，因此可以用于缓存、数据压缩等许多场景。

const Node = require('../../Node');

class MoveToFront {
    constructor() {
        this.head = null;
    }
    add(char) {
        this.head = new Node(char, this.head);
        let cur = this.head;
        while (cur) {
            const next = cur.next;
            if (!next) {
                return;
            }
            if (next.value == char) {
                cur.next = next.next;
                return;
            }
            cur = next;
        }
    }
    get data() {
        const data = [];
        let cur = this.head;
        while (cur) {
            data.push(cur.value);
            cur = cur.next;
        }
        return data;
    }
}

const m = new MoveToFront();
const chars = 'asdfadgs'.split('');
for (let char of chars) {
    m.add(char);
}
const data = m.data;
debugger
