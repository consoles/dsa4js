// 编写一个函数，接受一条链表的首结点作为参数，（破坏性地）将链表反转并返回结果链表的首结点
// Leetcode No206

/**
 * 迭代方式
 * @param {*} head 
 */
const reverse = head => {
    let prev = null;
    let cur = head;
    while (cur) {
        const next = cur.next;

        cur.next = prev;
        prev = cur;
        cur = next;
    }
    return prev;
};

/**
 * 递归方式：先颠倒最后n-1个节点。然后将原先链表的首节点插入到源链表的末端
 */
const reverse2 = head => {
    if (!head || !head.next) return head;
    const second = head.next;
    const newHead = reverse2(second);
    second.next = head;
    head.next = null;
    return newHead;
};

const { createLinkedList, printLinkedList } = require('../../util');
let head = createLinkedList([1, 2, 3, 4, 5]);
printLinkedList(head);
head = reverse2(head);
printLinkedList(head);