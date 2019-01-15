// 编写一个Queue的用例，接受一个命令行参数k并打印出标准输入中的倒数第k个字符串（假设标准输入中至少有k个字符串）。

const Queue = require('../../LinkedQueue');

const lastK = (arr, k) => {
    const q = new Queue();
    for (const item of arr) {
        q.enqueue(item);
    }

    for (let i = 0; i < arr.length - k; i++) {
        q.dequeue();
    }

    return q.dequeue();
}

const ret = lastK([1, 2, 3, 4], 2);
debugger