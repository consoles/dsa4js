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
 * @param {ListNode} l1
 * @param {ListNode} l2
 * @return {ListNode}
 */
var mergeTwoLists = function (l1, l2) {
  const dummyHead = new ListNode(-1);
  let cur = dummyHead;
  // while (l1 || l2) {
  //   let node = null;
  //   if (!l1) {
  //     node = l2;
  //     l2 = l2.next;
  //   } else if (!l2) {
  //     node = l1;
  //     l1 = l1.next;
  //   } else if (l1.val < l2.val) {
  //     node = l1;
  //     l1 = l1.next;
  //   } else {
  //     node = l2;
  //     l2 = l2.next;
  //   }
  //   cur.next = node;
  //   cur = node;
  // }

  // // 换一种写法
  // while (l1 && l2) {
  //   let node = null;
  //   if (l1.val <= l2.val) {
  //     node = l1;
  //     l1 = l1.next;
  //   } else {
  //     node = l2;
  //     l2 = l2.next;
  //   }
  //   cur.next = node;
  //   cur = node;
  // }
  // cur.next = l1 ? l1 : l2;
  // return dummyHead.next;

  // 递归法
  if (!l1) return l2;
  if (!l2) return l1;

  let head = null;
  if (l1.val <= l2.val) {
    head = l1;
    head.next = mergeTwoLists(l1.next, l2);
  } else {
    head = l2;
    head.next = mergeTwoLists(l1, l2.next);
  }
  return head;
};

const l1 = {
  val: 1,
  next: {
    val: 2,
    next: {
      val: 4
    }
  }
};

const l2 = {
  val: 1,
  next: {
    val: 3,
    next: {
      val: 4
    }
  }
};

l = mergeTwoLists(l1, l2);
debugger;
