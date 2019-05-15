const Node = require('./Node');

// 双向链表
class LinkedList {
  constructor(){
    this.head = null;
    this.last = null;
    this.size = 0;
  }
  append(value) {
      const node = new Node(value);
      if (!this.head) {
        this.head = this.last = node;
      } else {
        this.last = this.last.next = node;
      }
      this.size++;
  }
  [Symbol.iterator]() {
    let cur = this.head;
    return {
      next() {
        if (cur) {
          let value = cur.value;
          cur = cur.next;
          return { done: false, value };
        }
        return { done: true };
      }
    };
  }
}

module.exports = LinkedList;
