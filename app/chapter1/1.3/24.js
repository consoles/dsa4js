// 编写一个方法removeAfter() ，接受一个链表结点作为参数并删除该结点的后续结点（如果参数结点或参数结点的后续结点为空则什么也不做）

const removeAfter = (head, node) => {
    if (!node || !node.next) return head;
    node.next = node.next.next;
    return head;
};

const { createLinkedList, printLinkedList, findNode } = require('../../util');

let head = createLinkedList([1, 2, 3, 4, 5]);
printLinkedList(head);
const node = findNode(head, 1);
head = removeAfter(head, node);
printLinkedList(head);
