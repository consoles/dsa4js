// Josephus 问题。在这个古老的问题中，N 个身陷绝境的人一致同意通过以下方式减少生存人数。他们围坐成一圈（位置记作 0 到 N - 1）并从第一个人开始报数，报到 M 的人会被杀死，然后下一个人重置从1开始报数，直到最后一个人留下来。传说中 Josephus 找到了不会被杀死的位置。编写一个 Queue 的用例 Josephus，从命令行接受 N 和 M 并打印出人们被杀死的顺序（这也将显示 Josephus 在圈中的位置）

// https://blog.csdn.net/wusuopubupt/article/details/18214999

const Queue = require('../../LinkedQueue');
const q = new Queue();
const n = 5;
const m = 3;
for (let i = 1; i <= n; i++) {
    q.enqueue(i);
}
let count = 0;
while (!q.isEmpty()) {
    const item = q.dequeue();
    count++;
    if (count === m) {
        count = 0;
        console.log('kill', item);
    } else {
        q.enqueue(item);
    }
}

// 利用递推式，求出正确的位置 fn = (fn-1 + k) % n;
const solve = (n, k) => {
    if (n === 1) return 0;
    const a = solve(n - 1, k);
    const ret = (a + k) % n;
    return ret;
}

const solve2 = (n, k) => {
    let index = 0;
    for (let i = 2; i <= n; i++) {
        index = (index + k) % i;
    }
    return index;
};

const index = solve(n, m);
console.log('num = ', index + 1);