// 1.3.35 随机队列。随机队列能够存储一组元素并支持isEmpty,enqueue,dequeue,sample
// 1.3.36 随机迭代器。为随机队列编写一个迭代器，随机返回队列中的所有元素

const shuffle = arr => {
    for (let i = 0; i < arr.length; i++) {
        let j = i + Math.floor((arr.length - i) * Math.random());
        [arr[i], arr[j]] = [arr[j], arr[i]];
    }
}

class RandomQueue {
    constructor() {
        this.size = 0;
        this.data = [];
    }
    isEmpty() {
        return this.size === 0;
    }
    enqueue(item) {
        this.data[this.size++] = item;
    }
    /**
     * 删除并随机返回一个元素（取样，且不放回）
     */
    dequeue() {
        const index = Math.floor(Math.random() * this.size);
        const item = this.data[index];
        for (let i = index; i < this.size - 1; i++) {
            this.data[i] = this.data[i + 1];
        }
        this.size--;
        this.data.pop(); // 维护数组容量
        return item;
    }
    /**
     * 随机返回一个元素但是不删除它（取样且放回）
     */
    sample() {
        const index = Math.floor(Math.random() * this.size);
        return this.data[index];
    }

    [Symbol.iterator]() {
        let index = 0;
        shuffle(this.data);
        const data = this.data;
        const len = this.size;
        return {
            next() {
                return index < len ? { done: false, value: data[index++] } : { done: true };
            }
        }
    }
}

const q = new RandomQueue();
for (let i = 0; i < 10; i++) {
    q.enqueue(i);
}
let d = q.dequeue();
d = q.dequeue();
d = q.dequeue();
for (let item of q) {
    console.log(item);
}