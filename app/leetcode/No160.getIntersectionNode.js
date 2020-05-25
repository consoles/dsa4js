/**
 * Definition for singly-linked list.
 * function ListNode(val) {
 *     this.val = val;
 *     this.next = null;
 * }
 */

function ListNode(val) {
  this.val = val;
  this.next = null;
}

/**
 * @param {ListNode} headA
 * @param {ListNode} headB
 * @return {ListNode}
 */
var getIntersectionNode = function (headA, headB) {
  // 保存在两个栈中，然后从两个栈中同步取出元素，如果某一时刻元素不一样了，说明该节点的父节点是第一个共有元素
  // const stackA = [];
  // const stackB = [];
  // while (headA) {
  //   stackA.push(headA);
  //   headA = headA.next;
  // }
  // while (headB) {
  //   stackB.push(headB);
  //   headB = headB.next;
  // }
  // let parent = null;
  // while (stackA.length && stackB.length) {
  //   const node1 = stackA.pop();
  //   const node2 = stackB.pop();
  //   if (node1 !== node2) {
  //     return parent;
  //   }
  //   parent = node1;
  // }
  // return null;

  // 双指针交换
  let node1 = headA, node2 = headB;

  while (node1 !== node2) {
    node1 = node1 ? node1.next : headB;
    node2 = node2 ? node2.next : headA;
  }

  return node1;
};

node8 = {
  val: 8,
  next: {
    val: 4,
    next: {
      val: 5
    }
  }
};
A = {
  val: 4,
  next: {
    val: 1,
    next: node8
  }
};

B = {
  val: 5,
  next: {
    val: 0,
    next: {
      val: 1,
      next: node8
    }
  }
};

ret = getIntersectionNode(A, B);
debugger;
