// class MinStack {
//   constructor() {
//     // 数据栈
//     this.data = [];
//     // 单调栈（栈顶是最小元素）
//     // 入栈：元素小于当前元素入栈，否则不做操作
//     // 出栈：元素等于当前元素出栈，否则不操作
//     this.helper = [];
//   }
//
//   push(x) {
//     this.data.push(x);
//     const top = this.helper[this.helper.length - 1];
//     if (top === void 0 || x <= top) {
//       this.helper.push(x);
//     }
//   }
//
//   pop() {
//     const x = this.data.pop();
//     const top = this.helper[this.helper.length - 1];
//     if (x === top) {
//       this.helper.pop();
//     }
//     return x;
//   }
//
//   top() {
//     return this.data[this.data.length - 1];
//   }
//
//   getMin() {
//     return this.helper[this.helper.length - 1];
//   }
// }

// 使用链表实现，维护一个min字段，从此节点开始直到尾节点的最小值
class Node {
  constructor(val, min) {
    this.val = val;
    this.min = min;
    this.next = null;
  }
}

class MinStack {
  constructor() {
    this.head = null;
  }

  push(x) {
    const node = new Node(x,x);
    if (this.head) {
      node.min = Math.min(x,this.head.min);
      node.next = this.head;
    }
    this.head = node;
  }

  pop() {
    if (this.head) this.head = this.head.next;
  }

  top() {
    return this.head ? this.head.val: -1;
  }

  getMin() {
    return this.head ? this.head.min : -1;
  }
}

minStack = new MinStack();
// minStack.push(-2);
// minStack.push(0);
// minStack.push(-3);
// console.log(minStack.getMin());  // --> 返回 -3.
// minStack.pop();
// console.log(minStack.top());     // --> 返回 0.
// console.log(minStack.getMin());   //--> 返回 -2.

minStack.push(0);
minStack.push(1);
minStack.push(0);

console.log(minStack.getMin());
minStack.pop();
console.log(minStack.getMin());
