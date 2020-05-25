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
 * @return {ListNode}
 */
var reverseList = function (head) {

  // 迭代法
  // let prev = null,
  //   cur = head;
  // while (cur) {
  //   const next = cur.next;
  //   cur.next = prev;
  //   prev = cur;
  //   cur = next;
  // }
  // return prev;

  // 递归法
  if (!head || !head.next) return head;
  const next = head.next;
  const newHead = reverseList(next);
  next.next = head;
  head.next = null;
  return newHead;
};

l = {
  val: 1,
  next: {
    val: 2,
    next: {
      val: 3,
      next: {
        val: 4,
        next: {
          val: 5
        }
      }
    }
  }
};

l = reverseList(l);
debugger;
