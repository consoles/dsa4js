/**
 * Definition for singly-linked list.
 * function ListNode(val) {
 *     this.val = val;
 *     this.next = null;
 * }
 */

class ListNode {
  constructor(val) {
    this.val = val;
    this.next = null;
  }
}

/**
 * @param {ListNode} l1
 * @param {ListNode} l2
 * @return {ListNode}
 */
var mergeTwoLists = function (l1, l2) {
  // if (!l1 || !l2) return l1 || l2;
  // const dummyHead = new ListNode();
  // let cur = dummyHead;
  // while (l1 || l2) {
  //   if (!l1) {
  //     cur.next = l2;
  //     break;
  //   }
  //   if (!l2) {
  //     cur.next = l1;
  //     break;
  //   }
  //   if (l1.val <= l2.val) {
  //     cur.next = l1;
  //     l1 = l1.next;
  //   } else {
  //     cur.next = l2;
  //     l2 = l2.next;
  //   }
  //   cur = cur.next;
  // }
  // return dummyHead.next;

  // 分治法
  if (!l1 || !l2) return l1 || l2;

  let head = null;
  if (l1.val <= l2.val) {
    head = l1;
    l1.next = mergeTwoLists(l1.next, l2);
  } else {
    head = l2;
    l2.next = mergeTwoLists(l1, l2.next);
  }
  return head;
};
