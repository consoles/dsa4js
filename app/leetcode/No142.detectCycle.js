/**
 * Definition for singly-linked list.
 * function ListNode(val) {
 *     this.val = val;
 *     this.next = null;
 * }
 */

/**
 * @param {ListNode} head
 * @return {ListNode}
 */
var detectCycle = function (head) {

    // 方法1：hash表记录已经访问过的节点

    // let visited = new Set()
    // let cur = head
    // while (cur) {
    //     if (visited.has(cur)) {
    //         return cur
    //     }
    //     visited.add(cur)
    //     cur = cur.next
    // }
    // return null
};

const node2 = {
    val: 2,
    next: {
        val: 0,
        next: {
            val: -4
        }
    }
}

node2.next.next.next = node2

const head1 = {
    val: 3,
    next: node2
}

let res = detectCycle(head1)
debugger