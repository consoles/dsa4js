function ListNode(val) {
  this.val = val;
  this.next = null;
}

var removeNthFromEnd = function (head, n) {
  // let count = 0;
  // let cur = head;
  // while (cur) {
  //   cur = cur.next;
  //   count++;
  // }
  // const dummyHead = new ListNode(-1);
  // dummyHead.next = head;
  // let prev = dummyHead;
  // cur = head;
  // let cnt = 0;
  // while (cnt < count - n) {
  //   prev = cur;
  //   cur = cur.next;
  //   cnt++;
  // }
  // prev.next = cur.next;
  // return dummyHead.next;

  // 快慢指针
  const dummyHead = new ListNode(-1);
  dummyHead.next = head;
  let first = dummyHead, second = dummyHead;
  for (let i = 1; i <= n + 1; i++) {
    first = first.next;
  }
  while (first) {
    first = first.next;
    second = second.next;
  }
  second.next = second.next.next;
  return dummyHead.next;
};

l = {
  val: 1,
  next: {
    val: 2,
    // next: {
    //   val: 3,
    //   next: {
    //     val: 4,
    //     next: {
    //       val: 5
    //     }
    //   }
    // }
  }
};

let a = removeNthFromEnd(l, 2);
debugger;
