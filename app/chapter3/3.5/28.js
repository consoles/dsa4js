class Node {
  constructor(value,next,prev){
    this.value = value;
    this.next = next;
    this.prev = prev;
  }
}

class DoubleLinkedList {
  constructor(){
    this.head = null;
    this.tail  = null;
  }
  appendLast(value){
    const node = new Node(value,null,this.tail);
    if (!this.tail) {
      this.head = node;
    } else {
      this.tail.next = node;
    }
    this.tail = node;
  }
  deleteFirst(){
    const head = this.head;
    if (!head) return null;
    this.head = head.next;
    if (!this.head) {
      this.tail  = null;
    }
    return head.value;
  }
  toString(){
    const values = [];
    let cur = this.head;
    while (cur) {
      values.push(cur.value);
      cur = cur.next;
    }
    return values.join(',');
  }
}

/**
 * 其实可以直接采用ES6中的set数据结构
 * 此处用双向链表结合hash表
 */
class UniqQueue{
  constructor(){
    this.list = new DoubleLinkedList();
    this.map = new Map();
  }

  enqueue(value){
    if (this.map.has(value)) {
      return;
    }
    this.list.appendLast(value);
    this.map.set(value,true);
  }
  dequeue(){
    const value = this.list.deleteFirst();
    this.map.delete(value);
    return value;
  }

  toString(){
    return this.list.toString();
  }
}

const q = new UniqQueue();
q.enqueue(1);
q.enqueue(2);
q.enqueue(1);
q.enqueue(3);
console.log(q.toString());
console.log(q.dequeue());
console.log(q.toString());
q.enqueue(0);
console.log(q.toString());
