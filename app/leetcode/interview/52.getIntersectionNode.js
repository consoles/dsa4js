// 相交链表的第一个节点

/**
 * Definition for singly-linked list.
 * function ListNode(val) {
 *     this.val = val;
 *     this.next = null;
 * }
 */

/**
 * @param {ListNode} headA
 * @param {ListNode} headB
 * @return {ListNode}
 */
var getIntersectionNode = function (headA, headB) {

  // 使用辅助栈

  // const stackA = [];
  // const stackB = [];
  //
  // while (headA) {
  //   stackA.push(headA);
  //   headA = headA.next;
  // }
  // while (headB) {
  //   stackB.push(headB);
  //   headB = headB.next;
  // }
  //
  // let node = null;
  //
  // while (stackA.length && stackB.length) {
  //   const nodeA = stackA.pop();
  //   const nodeB = stackB.pop();
  //   if (nodeA === nodeB) {
  //     node = nodeA;
  //   } else {
  //     return node;
  //   }
  // }
  //
  // return node;

  // 双指针
  // let node1 = headA, node2 = headB;
  // while (node1 !== node2) {
  //   node1 = node1 ? node1.next : headB;
  //   node2 = node2 ? node2.next : headA;
  // }
  // return node1;

  // 保存第一个链表的地址
  // const set = new Set();
  // while (headA) {
  //   set.add(headA);
  //   headA = headA.next;
  // }
  // // O(N^2) => O(N)
  // while (headB) {
  //   if (set.has(headB)) {
  //     return headB;
  //   }
  //   headB = headB.next;
  // }
  // return null;

  // 去掉长度的差异
  let len1 = 0, len2 = 0;
  let cur1 = headA, cur2 = headB;
  while (cur1) {
    len1++;
    cur1 = cur1.next;
  }
  while (cur2) {
    len2++;
    cur2 = cur2.next;
  }
  let diff = 0;
  cur1 = null;
  if (len1 > len2) {
    cur1 = headA;
    cur2 = headB;
    diff = len1 - len2;
  } else if (len1 < len2) {
    cur1 = headB;
    cur2 = headA;
    diff = len2 - len1;
  }
  while (diff--) {
    cur1 = cur1.next;
  }
  while (cur1 && cur2) {
    if (cur1 === cur2) {
      return cur1;
    }
    cur1 = cur1.next;
    cur2 = cur2.next;
  }
  return null;
};

const commonList = {
  val:8,
  next:{
    val:4,
    next:{
      val:5
    }
  }
};
const headA = {
  val:4,
  next:{
    val:1,
    next:commonList
  }
};
const headB = {
  val:5,
  next:{
    val:0,
    next:{
      val:1,
      next:commonList
    }
  }
};

const ret = getIntersectionNode(headA,headB);

console.log(ret);
