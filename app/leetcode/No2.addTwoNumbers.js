// 给出两个 非空 的链表用来表示两个非负的整数。其中，它们各自的位数是按照 逆序 的方式存储的，并且它们的每个节点只能存储 一位 数字。
//
// 如果，我们将这两个数相加起来，则会返回一个新的链表来表示它们的和。
//
// 您可以假设除了数字 0 之外，这两个数都不会以 0 开头。
//
// 示例：
//
// 输入：(2 -> 4 -> 3) + (5 -> 6 -> 4)
// 输出：7 -> 0 -> 8
// 原因：342 + 465 = 807
//
// 来源：力扣（LeetCode）
// 链接：https://leetcode-cn.com/problems/add-two-numbers
//   著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。

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
  const root = new ListNode(-1);
  // let cur = root;
  // let carry = 0; // 进位
  // let p = l1, q = l2;
  // while (p || q) {
  //   const x = p ? p.val : 0;
  //   const y = q ? q.val : 0;
  //   const sum = carry + x + y;
  //   carry = sum >= 10 ? 1 : 0;
  //   // 用取余就不用区分小于10和大于10了
  //   cur.next = new ListNode(sum % 10);
  //   if (p) p = p.next;
  //   if (q) q = q.next;
  //   cur = cur.next;
  // }
  //
  // // 如果都加完了，最高位有进位
  // if (carry) {
  //   cur.next = new ListNode(carry);
  // }

  // 优化
  let cur = root;
  let num = 0;
  while (l1 || l2 || num) {
    if (l1) {
      num += l1.val;
      l1 = l1.next;
    }
    if (l2) {
      num += l2.val;
      l2 = l2.next;
    }
    cur.next = new ListNode(num % 10);
    cur = cur.next;
    num = parseInt(num / 10);
  }

  return root.next;
};

const l1 = {
  val: 1

};

const l2 = {
  val: 9,
  next: {
    val: 9
  }
};

const ret = addTwoNumbers(l1, l2);
console.log(ret);

const ll1 = {
  val: 2,
  next: {
    val: 4,
    next: {
      val: 3
    }
  }
}

const ll2 = {
  val: 5,
  next: {
    val: 6,
    next: {
      val: 4
    }
  }
}

const ret2 = addTwoNumbers(ll1, ll2);
console.log(ret2);
