// 编写一个方法find() ，接受一条链表和一个字符串key作为参数。如果链表中的某个结点的item域的值为key，则方法返回true，否则返回false。

const find = (head, key) => {
    let cur = head;
    while (cur) {
        if (cur.value == key) return true;
        cur = cur.next;
    }
    return false;
};

const { createLinkedList, printLinkedList } = require('../../util');

const head = createLinkedList([1, 2, 3, 4, 5]);
const flag = find(head, 6);
debugger