// 编写一个方法remove() ，接受一个链表和一个字符串key作为参数，删除链表中所有item域为key的结点。

const remove = (head, key) => {
    const dummyHead = {
        next: head
    };
    let prev = dummyHead;
    let cur = prev.next;
    while (cur) {
        if (cur.value == key) {
            prev.next = cur.next;
        } else {
            prev = cur;
        }
        cur = cur.next;
    }
    return dummyHead.next;
};

const { createLinkedList, printLinkedList } = require('../../util');

let head = createLinkedList([1, 1, 2, 1, 1, 4]);
head = remove(head, 1);
printLinkedList(head);