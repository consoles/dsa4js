// 编写一个方法max() ，接受一个链表的首结点作为参数，返回链表中键最大的节点的值。假设所有键均为正整数，如果链表为空则返回0

const max = head => {
    let max = 0;
    let cur = head;
    while (cur) {
        max = Math.max(cur.value, max);
        cur = cur.next;
    }
    return max;
};

const { createLinkedList } = require('../../util');
let head = createLinkedList([1, 1, 2, 1, 1, 4]);
const m = max(head);
debugger