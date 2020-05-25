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
var deleteDuplicates = function (head) {
  let cur = head;
  while (cur) {
    const val = cur.val;
    let next = cur.next;
    while (next && next.val === val) {
      next = next.next;
    }
    cur.next = next;
    cur = next;
  }
  return head;
};

// h = {
//   val: 1,
//   next: {
//     val: 1,
//     next: {
//       val: 2
//     }
//   }
// };

h = {
  val: 1,
  next: {
    val: 1,
    next: {
      val: 2,
      next: {
        val: 3,
        next: {
          val: 3
        }
      }
    }
  }
};

deleteDuplicates(h);
