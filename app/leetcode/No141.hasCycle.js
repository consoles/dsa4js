/**
 * Definition for singly-linked list.
 * function ListNode(val) {
 *     this.val = val;
 *     this.next = null;
 * }
 */

/**
 * @param {ListNode} head
 * @return {boolean}
 */
var hasCycle = function (head) {
    // 方式1：记录所有访问过的节点
    // const visited = new Set()
    // let cur = head
    // while (cur) {
    //     if (visited.has(cur)) {
    //         return true
    //     }
    //     visited.add(cur)
    //     cur = cur.next
    // }
    // return false

    // 方式2：快慢指针
    // 快慢指针位于同一起点，快指针每次走2步，慢指针每次走1步，如果链表存在环路，则快指针一定比慢指针先穿过环，反过来追慢指针，有点类似龟速赛跑
    // 注意这里快慢指针位于同一起点，为了保持循环不变量用了do-while循环
    // let fast = head, slow = head
    // do {
    //     // 如果链表存在尾，则一定不会有环路
    //     if (!fast || !fast.next) return false
    //     fast = fast.next.next
    //     slow = slow.next
    // } while (fast !== slow)
    // return true

    // 用while循环
    if (!head || !head.next) return false
    let slow = head, fast = head.next
    while (slow !== fast) {
        if (!fast || !fast.next) return false
        fast = fast.next.next
        slow = slow.next
    }
    return true
};

const node2 = {
    val: 2
}

let head = {
    val: 3,
    next: node2
}
node2.next = {
    val: 0,
    next: {
        val: -4,
        next: node2
    }
}

let flag = hasCycle(head)
console.log(flag);