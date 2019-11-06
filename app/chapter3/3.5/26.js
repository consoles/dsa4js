class Node {
  constructor(value, prev, next) {
    this.value = value;
    this.prev = prev;
    this.next = next;
  }
}

class DoubleLinkedList {

  constructor() {
    this.head = null;
    this.tail = null;
  }

  insertFirst(value) {
    if (!this.head) {
      this.head = this.tail = new Node(value);
    } else {
      const newNode = new Node(value, null, this.head);
      this.head.prev = newNode;
      this.head = newNode;
    }
    return this.head;
  }

  deleteLast() {
    if (!this.tail) return null;

    const ret = this.tail;

    const newTail = this.tail.prev;

    if (!newTail) {
      this.head = null;
    } else {
      newTail.next = null;
    }

    return ret.value;
  }

  // 把节点移动到头部
  moveToFront(node) {
    if (!node || node === this.head) return; // 已经在头部了
    const {prev, next} = node;
    // 这个判断其实没有必要，因为前面已经判断过不是头结点了，前驱节点一定存在,写这个的目的是
    if (prev) {
      prev.next = next;
    }
    if (next) {
      next.prev = prev;
    }
    node.prev = null;
    node.next = this.head;

    this.head.prev = node;
    this.head = node;
  }

}

class LRU {

  constructor() {
    this.list = new DoubleLinkedList();
    this.map = new Map(); // key => 元素 ；value，双向链表中的节点
  }

  /**
   * 将不存在数据结构中的元素插入
   * @param value
   */
  access(value) {
    let node = this.map.get(value);
    if (!node) {
      node = this.list.insertFirst(value);
      this.map.set(value, node);
    } else {
      this.list.moveToFront(node);
    }
  }

  /**
   * 删除并返回最近最少访问的元素
   */
  delete() {
    const value = this.list.deleteLast();
    this.map.delete(value);
    return value;
  }

  toString() {
    let cur = this.list.head;
    const values = [];
    while (cur) {
      values.push(cur.value);
      cur = cur.next;
    }
    return values.join(' -> ');
  }
}

const cache = new LRU();

cache.access(7);
console.log(cache.toString());
cache.access(0);
console.log(cache.toString());
cache.access(1);
console.log(cache.toString());
cache.access(2);
console.log(cache.toString());
cache.access(0);
console.log(cache.toString());
cache.access(3);
console.log(cache.toString());
cache.access(0);
console.log(cache.toString());
console.log('delete last',cache.delete());
console.log(cache.toString());
cache.access(4);
console.log(cache.toString());
