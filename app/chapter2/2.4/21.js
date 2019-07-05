const swap = require('../../swap');

class Node {
  constructor(key, value) {
    this.key = key;
    this.value = value;
  }
}

class MaxPQ {
  constructor() {
    this.arr = [-1];
    this.num = 0;
    this.sz = 0;
  }

  less(i, j) {
    return this.arr[i].key < this.arr[j].key;
  }

  _swim(k) {
    while (k > 1) {
      const parentIndex = parseInt(k / 2);
      if (!this.less(parentIndex, k)) break;
      swap(this.arr, k, parentIndex);
      k = parentIndex;
    }
  }

  _sink(k) {
    const sz = this.sz;
    while (k < sz) {
      let j = 2 * k;
      if (j > sz) break;
      if (j + 1 < sz && this.less(j, j + 1)) j++;
      if(!this.less(k,j)) break;
      swap(this.arr, j, k);
      k = j;
    }
  }

  getKey(){
    return this.num++;
  }

  insert(value) {
    const key = this.getKey();
    const node = new Node(key, value);
    this.arr.push(node);
    this.sz++;
    this._swim(this.sz);
  }

  delMax() {
    const value = this.arr[1].value;
    swap(this.arr, 1, this.sz--);
    this._sink(1);
    return value;
  }

  isEmpty(){
    return this.sz === 0;
  }
}

class MinPQ extends MaxPQ {
  less(i,j) {
    return this.arr[j].value < this.arr[i].value;
  }
}

// const q = new MinPQ();
// q.insert(1);
// q.insert(3);
// q.insert(2);
// q.insert(4);
// q.insert(5);
// debugger;

// 栈(后进先出)，后进的元素比较大
class Stack {
  constructor() {
    this.q = new MaxPQ();
  }

  isEmpty(){
    return this.q.isEmpty();
  }

  push(value) {
    this.q.insert(value);
  }

  pop() {
    return this.q.delMax();
  }
}

// const s = new Stack();
// s.push(1);
// s.push(2);
// s.push(3);
//
// while (!s.isEmpty()) {
//   console.log(s.pop());
// }

// 队列（先进先出），先进的元素比较大
class Queue{
  constructor(){
    this.pq = new MinPQ();
  }
  isEmpty(){
    return this.pq.isEmpty();
  }
  enqueue(value){
    this.pq.insert(value);
  }
  dequeue(){
    return this.pq.delMax();
  }
}

// const q = new Queue();
// q.enqueue(1);
// q.enqueue(2);
// q.enqueue(3);
//
// while (!q.isEmpty()) {
//   console.log(q.dequeue());
// }

class RandomPQ extends MaxPQ {
  getKey() {
    return Math.random();
  }
}

// 随机队列
class RandomQueue extends Queue{
  constructor(){
    super();
    this.pq = new RandomPQ();
  }
}

const q = new RandomQueue();
q.enqueue(1);
q.enqueue(2);
q.enqueue(3);

while (!q.isEmpty()) {
  console.log(q.dequeue());
}
