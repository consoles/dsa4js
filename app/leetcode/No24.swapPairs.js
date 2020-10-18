/**
 * Definition for singly-linked list.
 * function ListNode(val, next) {
 *     this.val = (val===undefined ? 0 : val)
 *     this.next = (next===undefined ? null : next)
 * }
 */
/**
 * @param {ListNode} head
 * @return {ListNode}
 */
var swapPairs = function(head) {
    // // 递归终止条件：链表中没有节点或者链表中只有一个节点，此时无法进行交换
    // if (!head || !head.next) return head
    // // 如果链表中至少有2个节点，则在两两交换链表中的节点之后，原始链表的头结点变为新链表的第二个节点，原始链表的第二个节点变成新链表的头结点（因此最后的返回值是second）。
    // // 链表中其余节点的两两交换可以递归地实现。
    // const first = head
    // const second = head.next
    // first.next = swapPairs(second.next)
    // second.next = first
    // return second

    // 迭代
    let dummyHead = {val:-1,next:head}
    let cur = dummyHead
    while(cur.next && cur.next.next) {
        const start = cur.next
        const end = cur.next.next
        cur.next = end
        start.next = end.next
        end.next = start
        cur = start
    }
    return dummyHead.next
};

const {buildLinkedListFromArray,dumyLinkedList} = require('./utils')
const head =  buildLinkedListFromArray([1,2,3,4])
// 注意newHead是反转链表的返回值，原先的head节点是有副作用的
const newHead = swapPairs(head)
console.log( dumyLinkedList(newHead) );