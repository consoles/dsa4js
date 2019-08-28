class Node {
  constructor(value, next) {
    this.value = value;
    this.next = next;
  }
}

// 向单链表中插入节点

/**
 * 非递归实现
 */
function linkedListInsert1(head, value) {
  if (!head) return new Node(value);
  let cur = head;
  while (cur.next) {
    cur = cur.next;
  }
  cur.next = new Node(value);
  return head;
}

/**
 * 递归实现
 */
function linkedListInsert2(head, value) {
  if (!head) return new Node(value);
  head.next = linkedListInsert2(head.next, value);
  return head;
}

// const head = linkedListInsert1(null,0);
// const arr = [1,2,3,4,5];
// for (const value of arr) {
//   linkedListInsert1(head,value);
// }

const head = linkedListInsert2(null, 0);
const arr = [1, 2, 3, 4, 5];
for (const value of arr) {
  linkedListInsert2(head, value);
}
debugger;
