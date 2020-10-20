/**
 * Definition for singly-linked list.
 * function ListNode(val, next) {
 *     this.val = (val===undefined ? 0 : val)
 *     this.next = (next===undefined ? null : next)
 * }
 */
/**
 * @param {ListNode} head
 * @return {void} Do not return anything, modify head in-place instead.
 */
var reorderList = function (head) {
  if (!head) return;
  // // 方法1：模拟，按照指定顺序重建链表
  // const nodes = [];
  // let cur = head;
  // while (cur) {
  //   nodes.push(cur);
  //   cur = cur.next;
  // }
  // // 链表重建
  // let i = 0, j = nodes.length - 1;
  // while (i < j) {
  //   nodes[i].next = nodes[j];
  //   i++;
  //   if (i === j) {
  //     break;
  //   }
  //   nodes[j].next = nodes[i];
  //   j--;
  // }
  // nodes[i].next = null;

  // 方法2：寻找链表中间节点（快慢指针） + 链表逆序 + 合并链表

  function middleNode(head) {
    let slow = head
    let fast = head
    // 快慢指针同时出发，快指针走2步，慢指针走一步
    while (fast.next && fast.next.next) {
      fast = fast.next.next
      slow = slow.next
    }
    return slow
  }

  function reverseList(head) {
    let prev = null
    let cur = head
    while (cur) {
      const next = cur.next
      cur.next = prev
      prev = cur
      cur = next
    }
    return prev
  }

  function mergeList(l1, l2) {
    while (l1 && l2) {
      const l1Next = l1.next
      const l2Next = l2.next

      l1.next = l2
      l1 = l1Next

      l2.next = l1
      l2 = l2Next
    }
  }

  const mid = middleNode(head)
  let l1 = head
  let l2 = mid.next
  mid.next = null
  l2 = reverseList(l2)
  mergeList(l1, l2)
};

const {buildLinkedListFromArray} = require('./utils')
let head = buildLinkedListFromArray([1, 2, 3, 4])
reorderList(head)
debugger
head = buildLinkedListFromArray([1, 2, 3, 4, 5])
reorderList(head)
debugger
