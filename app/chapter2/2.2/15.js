// 自底向上的有序队列的归并排序
// 给定N个元素，创建N个队列，每个队列包含其中1个元素。创建一个由这N个队列组成的队列，最后不断用联系2.2.14中的方法将队列的头两个元素归并，并将结果重新加入到队列结尾，直到队列只剩下一个元素为止

const Queue = require('../../LinkedQueue');

function merge(q1, q2) {
  const q = new Queue();
  while (!q1.isEmpty() || !q2.isEmpty()) {
    if (q1.isEmpty()) {
      q.enqueue(q2.dequeue());
    } else if (q2.isEmpty()) {
      q.enqueue(q1.dequeue());
    } else if (q1.getHead() < q2.getHead()) {
      q.enqueue(q1.dequeue());
    } else {
      q.enqueue(q2.dequeue());
    }
  }
  return q;
}

function sort(arr) {
  const q = new Queue();
  for (const item of arr) {
    const tmpQueue = new Queue();
    tmpQueue.enqueue(item);
    q.enqueue(tmpQueue);
  }
  while (q.size > 1) {
    const q1 = q.dequeue();
    const q2 = q.dequeue();
    const q3 = merge(q1, q2);
    q.enqueue(q3);
  }
  const tmpQ = q.dequeue();
  const ret = [];
  for (const item of tmpQ) {
    ret.push(item);
  }
  return ret;
}

const arr = [2, 1, 3, 4, 5, 7, 6];
const ret = sort(arr);
debugger;
