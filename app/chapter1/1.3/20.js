// 编写一个方法delete() ，接受一个int参数k，删除链表的第k个元素（如果它存在的话）。

const { createLinkedList, printLinkedList } = require('../../util');

const deleteK = (head, k) => {

    if (k <= 0) return head;

    const dummyHead = {
        next: head
    };

    let cur = dummyHead;
    let count = 1;

    while (count < k) {
        cur = cur.next;
        if (!cur) {
            return head;
        }
        count++;
    }

    if (cur.next) {
        cur.next = cur.next.next;
    }

    return dummyHead.next;
};

let head = createLinkedList([1, 2, 3, 4, 5]);
printLinkedList(head);
head = deleteK(head, 5);
printLinkedList(head);
