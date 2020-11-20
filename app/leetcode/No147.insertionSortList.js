/**
 * Definition for singly-linked list.
 * function ListNode(val) {
 *     this.val = val;
 *     this.next = null;
 * }
 */

const { buildLinkedListFromArray } = require("./utils");

/**
 * @param {ListNode} head
 * @return {ListNode}
 */
var insertionSortList = function (head) {
    const dummyHead = { val: -1, next: head };
    let cur = dummyHead.next; // 当前已排序链表的最后一个
    while (cur && cur.next) {
        // 跳过有序元素
        if (cur.next.val >= cur.val) {
            cur = cur.next;
            continue;
        }
        const next = cur.next;
        const val = next.val;
        // 找到正确插入位置
        let prev = dummyHead;
        while (prev.next.val < val) {
            prev = prev.next;
        }
        cur.next = next.next;
        next.next = prev.next;
        prev.next = next;
    }
    return dummyHead.next;
};

const head = buildLinkedListFromArray([4, 2, 1, 3])
const newHead = insertionSortList(head)
debugger