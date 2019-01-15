// 编写一个方法insertAfter() ，接受两个链表结点作为参数，将第二结点插入链表并使之成为第一个结点的后续结点（如果两个参数为空则什么也不做）。

const insertAfter = (node1, node2) => {
    if (!node1 || !node2) return;
    node2.next = node1.next;
    node1.next = node2;
}

const { createLinkedList, printLinkedList, findNode } = require('../../util');
const Node = require('../../Node');

const head = createLinkedList([1, 2, 3, 4, 5]);
printLinkedList(head);

const node1 = findNode(head, 5);
const node2 = new Node(-1);
insertAfter(node1, node2);
printLinkedList(head);