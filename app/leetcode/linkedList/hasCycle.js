function ListNode(val) {
  this.val = val;
  this.next = null;
}

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

  // 用集合保存已经被访问过的节点

  // const visited = new Set();
  // let cur = head;
  // while (cur) {
  //   if (visited.has(cur)) {
  //     return true;
  //   }
  //   visited.add(cur);
  //   cur = cur.next;
  // }
  // return false;

  // 快慢指针，快指针向后2步，慢指针向后一步，如果存在环则快慢指针一定会相遇
  if (!head || !head.next) return false;

  let slow = head,
    fast = head.next;

  while (slow !== fast) {
    if (!fast || !fast.next) {
      return false;
    }
    slow = slow.next;
    fast = fast.next.next;
  }
  return true;
};

const node2 = {val: 2, next: null};

l1 = {
  val: 1,
  next: {
    val:2
  }
};

node2.next = {
  val: 0,
  next: {
    val: 4,
    next: node2
  }
};

ret = hasCycle(l1);
debugger;
