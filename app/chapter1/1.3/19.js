// 给出一段代码，删除链表的尾结点，其中链表的首结点为first。

const { createLinkedList, printLinkedList } = require('../../util');

const deleteLastNode = first => {

    if (!first) return null;
    if (!first.next) return first = null;

    let cur = first;

    while (cur.next.next) {
        cur = cur.next;
    }

    cur.next = null;

    return first;
};

// 引入虚拟头结点
const deleteLastNode2 = first => {

    const dummyHead = {
        next: first
    };

    let cur = dummyHead;
    while (cur.next.next) {
        cur = cur.next;
    }
    cur.next = null;
    return dummyHead.next;
};

let first = createLinkedList([1]);
printLinkedList(first);
first = deleteLastNode(first);
printLinkedList(first);
