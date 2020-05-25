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
var isPalindrome = function (head) {

  // 只想到了转化成数组的方法
  // let arr = [];
  // let cur = head;
  // while (cur) {
  //   arr.push(cur.val);
  //   cur = cur.next;
  // }
  // let i = 0, j = arr.length - 1;
  // while (i < j) {
  //   if (arr[i] !== arr[j]) {
  //     return false;
  //   }
  //   i++;
  //   j--;
  // }
  // return true;

  // 递归
  // let frontPointer = head;
  //
  // function recursiveCheck(curNode) {
  //   if (curNode) {
  //     // 这个检测放在第一行保证递归到最深处
  //     if (!recursiveCheck(curNode.next)) return false;
  //     if (curNode.val !== frontPointer.val) return false;
  //     frontPointer = frontPointer.next;
  //   }
  //   return true;
  // }
  //
  // return recursiveCheck(head);

  // 反转后半部分链表
  if (!head) return true;

  const firstHalfEnd = endOfFirstHalf(head);
  const secondHalfStart = reverseList(firstHalfEnd.next);

  let p1 = head;
  let p2 = secondHalfStart;

  let result = true;
  while (p2) {
    if (p1.val !== p2.val) {
      result = false;
      break;
    }
    p1 = p1.next;
    p2 = p2.next;
  }

  firstHalfEnd.next = reverseList(secondHalfStart);
  return result;

  function reverseList(head) {
    let prev = null,
      cur = head;
    while (cur) {
      const next = cur.next;
      cur.next = prev;
      prev = cur;
      cur = next;
    }
    return prev;
  }

  function endOfFirstHalf(head) {
    let fast = head,
      slow = head;
    while (fast.next && fast.next.next) {
      fast = fast.next.next;
      slow = slow.next;
    }
    return slow;
  }
};

function print(node) {
  if (!node) return;
  print(node.next);
  console.log(node.val);
}

// l = {
//   val: 1,
//   next: {
//     val: 2,
//   }
// };
//
// l = isPalindrome(l);
// debugger;

l = {
  val: 1,
  next: {
    val: 2,
    next: {
      val: 3,
      next: {
        val: 4
      }
    }
  }
};

print(l);

// l = isPalindrome(l);
// debugger;
