// 归并有序的队列
// 将2个有序队列作为参数，返回一个归并后的有序队列

const Queue = require('../../LinkedQueue');

const q1 = new Queue();
const q2 = new Queue();

const arr1 = [1, 2, 3, 4, 5];
const arr2 = [-5, 2, 4, 7, 8];

for (const item of arr1) {
  q1.enqueue(item);
}
for (const item of arr2) {
  q2.enqueue(item);
}

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

const q = merge(q1, q2);
for (const item of q) {
  console.log(item);
}
debugger;
