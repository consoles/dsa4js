// 1.3.27的递归解法
const max = head => !head ? 0 : Math.max(head.value, max(head.next));

const { createLinkedList } = require('../../util');
let head = createLinkedList([1, 1, 2, 1, 1, 4]);
const m = max(head);
debugger