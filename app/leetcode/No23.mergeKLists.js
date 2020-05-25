function ListNode(val) {
  this.val = val;
  this.next = null;
}

/**
 * 最大堆实现优先队列
 */
class PriorityQueue {
  constructor(cmpFun) {
    this.cmpFun = cmpFun;
    // 最大堆，索引从1开始计数，左孩子2*k,右孩子2*k+1,父节点k/2
    this.data = [-1];
  }

  _swap(i, j) {
    [this.data[i], this.data[j]] = [this.data[j], this.data[i]];
  }

  _swim(k) {
    while (k > 1) {
      const p = parseInt(k / 2);
      if (this.cmpFun(this.data[k], this.data[p]) > 0) {
        this._swap(k, p);
        k = p;
      } else {
        break;
      }
    }
  }

  _sink(k) {
    while (2 * k < this.data.length) {
      let j = 2 * k;
      if (j + 1 < this.data.length && this.cmpFun(this.data[j + 1], this.data[j]) > 0) {
        j++;
      }
      if (this.cmpFun(this.data[j], this.data[k]) > 0) {
        this._swap(j, k);
        k = j;
      } else {
        break;
      }
    }
  }

  offer(e) {
    this.data.push(e);
    const k = this.data.length - 1;
    this._swim(k);
  }

  isEmpty() {
    return this.data.length === 1;
  }

  clear() {
    this.data = [-1];
  }

  poll() {
    const e = this.data[1];
    if (!e) return e;
    const len = this.data.length - 1;
    this._swap(1, len);
    this.data.length = len;
    this._sink(1);
    return e;
  }
}

/**
 * Definition for singly-linked list.
 * function ListNode(val) {
 *     this.val = val;
 *     this.next = null;
 * }
 */
/**
 * @param {ListNode[]} lists
 * @return {ListNode}
 */
var mergeKLists = function (lists) {

  // 方法1：不断将两个链表进行合并

  // // 不断合并2个链表
  // function mergeTwoLists(l1, l2) {
  //   // if (!l1) return l2;
  //   // if (!l2) return l1;
  //   // 简化
  //   if (!l1 || !l2) return l1 || l2;
  //
  //   const dummyHead = new ListNode();
  //   let cur = dummyHead;
  //   while (l1 || l2) {
  //     if (!l1) {
  //       cur.next = l2;
  //       break;
  //     } else if (!l2) {
  //       cur.next = l1;
  //       break;
  //     } else if (l1.val < l2.val) {
  //       cur.next = l1;
  //       l1 = l1.next;
  //     } else {
  //       cur.next = l2;
  //       l2 = l2.next;
  //     }
  //     cur = cur.next;
  //   }
  //   return dummyHead.next;
  // }
  //
  // if (lists.length === 0) return null;
  // if (lists.length === 1) return lists[0];
  // // let head = null;
  // // for (const h of lists) {
  // //   head = mergeTwoLists(head, h);
  // // }
  // // return head;
  //
  // // 方法2：分治归并，类似与归并排序的思想，每次两两归并的链表规模是差不多的
  // function merge(l, r) {
  //   // 注意这两个条件和归并排序的归并不同，那个不需要排序可以直接return，我们这里要有返回值的
  //   if (l > r) return null;
  //   if (l === r) return lists[l];
  //   const mid = (l + r) >> 1;
  //   return mergeTwoLists(merge(l, mid), merge(mid + 1, r));
  // }
  //
  // return merge(0, lists.length - 1);

  // 方法3：在n个链表中寻找最小的头结点，然后将对应的头结点向后移动
  // const n = lists.length;
  // const dummyHead = new ListNode();
  // let cur = dummyHead;
  // while (true) {
  //   let minNode = null;
  //   let minIndex = -1;
  //   for (let i = 0; i < n; i++) {
  //     const node = lists[i];
  //     if (!node) continue;
  //     if (!minNode || node.val < minNode.val) {
  //       minNode = node;
  //       minIndex = i;
  //     }
  //   }
  //   if (!minNode) {
  //     break;
  //   }
  //   cur = cur.next = minNode;
  //   lists[minIndex] = lists[minIndex].next;
  // }
  // return dummyHead.next;

  // 维护n个元素的优先队列，取出元素的时候同时向队列中加入这个元素的下一个元素
  const q = new PriorityQueue((a, b) => b.val - a.val); // 此优先队列的实现是最大堆，所以比较函数反着传
  for (const node of lists) {
    q.offer(node);
  }
  const dummyHead = new ListNode();
  let cur = dummyHead;
  while (!q.isEmpty()) {
    const node = q.poll();
    if (!node) {
      break;
    }
    cur = cur.next = node;
    if (node.next) {
      q.offer(node.next);
    }
  }
  return dummyHead.next;
};

const lists = [
  {
    val: 1,
    next: {
      val: 4,
      next: {
        val: 5
      }
    },
  },
  {
    val: 1,
    next: {
      val: 3,
      next: {
        val: 4
      }
    },
  },
  {
    val: 2,
    next: {
      val: 6,
    },
  },
];

const l = mergeKLists(lists);
debugger;
