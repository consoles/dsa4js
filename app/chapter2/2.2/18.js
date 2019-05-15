// 打乱链表

// 实现一个分治算法，使用线性对数级别的时间和对数级别的额外空间随机打乱一条链表

const Node = require('../../Node');

const {createLinkedList} = require('../../util');

function mergeList(list1, list2) {
  const dummyHead = new Node(-1, list1); // 引入list1的辅助头结点 dummyHead，因为可能在头部插入
  let cur1 = dummyHead,
    cur2 = list2;

  while (cur1.next && cur2) {
    if (Math.random() > .5) {
      list2 = cur2.next;
      cur2.next = cur1.next;
      cur1.next = cur2;
      cur1 = cur2;
      cur2 = list2;
    } else {
      cur1 = cur1.next;
    }
  }
  if (!cur1.next) {
    cur1.next = cur2;
  }
  return dummyHead.next;
}

function shuffleLinkedList(head) {
  if (!head || !head.next) return head;

  // 使用快慢指针寻找中间节点的位置
  let slow = head,
    fast = head;

  while (fast.next && fast.next.next) {
    fast = fast.next.next;
    slow = slow.next;
  }

  let leftHead = head,
    rightHead = slow.next;

  slow.next = null; // 左半部分链表的next域清空，先用一个变量记录右边链表的头

  leftHead = shuffleLinkedList(leftHead);
  rightHead = shuffleLinkedList(rightHead);

  return mergeList(leftHead, rightHead);
}

const list = createLinkedList([2, 1, 3, 5, 4]);
const ret = shuffleLinkedList(list);
debugger;
