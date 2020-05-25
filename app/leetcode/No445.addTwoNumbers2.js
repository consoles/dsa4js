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
var addTwoNumbers = function (l1, l2) {

  if (!l1) return l2;
  if (!l2) return l1;

  // 翻转链表，相加，然后再翻转
  // function reverseList(head) {
  //   let prev = null;
  //   let cur = head;
  //   while (cur) {
  //     const next = cur.next;
  //     cur.next = prev;
  //     prev = cur;
  //     cur = next;
  //   }
  //   return prev;
  // }
  //
  // l1 = reverseList(l1);
  // l2 = reverseList(l2);
  //
  // const dummyHead = new ListNode(-1);
  //
  // let cur = dummyHead;
  //
  // let carry = 0;

  // while (l1 && l2) {
  //   const sum = l1.val + l2.val + carry;
  //   carry = parseInt(sum / 10);
  //   cur =  cur.next  = new ListNode(sum % 10);
  //   l1 =  l1.next;
  //   l2 = l2.next;
  // }
  // if (carry === 0) {
  //   cur.next = l1 ? l1 : l2;
  // } else {
  //   let l = l1 ? l1 : l2;
  //   while (carry) {
  //     let sum = carry;
  //     if (!l) {
  //       cur = cur.next  = new ListNode(carry);
  //       break;
  //     }
  //     sum = l.val + carry;
  //     carry = parseInt(sum / 10);
  //     cur =  cur.next  = new ListNode(sum % 10);
  //     l = l.next;
  //   }
  //   cur.next = l;
  // }

  // 上面代码的优化，虽然代码少了，但是性能不如上面的，上面的代码当一个链表比较短的时候可以提前终止
  // while (l1 || l2) {
  //   const x = l1 ? l1.val : 0;
  //   const y = l2 ? l2.val : 0;
  //   const sum = x + y + carry;
  //   cur = cur.next = new ListNode(sum % 10);
  //   carry = parseInt(sum / 10);
  //   if (l1) l1 = l1.next;
  //   if (l2) l2 = l2.next;
  // }
  // if (carry) cur.next = new ListNode(carry);
  //
  // return reverseList(dummyHead.next);

  // 用两个栈
  const stack1 = [], stack2 = [];
  let cur = l1;
  while (cur) {
    stack1.push(cur.val);
    cur = cur.next;
  }
  cur = l2;
  while (cur) {
    stack2.push(cur.val);
    cur = cur.next;
  }
  let carry = 0;
  cur = null;
  while (stack1.length || stack2.length || carry) {
    const x = stack1.length ? stack1.pop() : 0;
    const y = stack2.length ? stack2.pop() : 0;
    const sum = x + y + carry;
    carry = parseInt(sum / 10);
    // 头插法构建链表，可以不翻转了
    const node = new ListNode(sum % 10);
    node.next = cur;
    cur = node;
  }

  return cur;
};

l1 = {
  val: 9,
  next: {
    val: 9,
    next: {
      val: 9,
      next: {
        val: 9
      }
    }
  }
};

l2 = {
  val: 5,
  next: {
    val: 6,
    next: {
      val: 4
    }
  }
};

l = addTwoNumbers(l1, l2);
debugger;
