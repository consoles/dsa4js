// 1.3.34 随机背包 编写一个 RandomBag 类来实现isEmpty,size,add。请注意，除了形容词随机之外，这份 API 和 Bag 的 API 是相同的，这意味着迭代应该随机访问背包中的所有元素(对于每次迭代，所有的 N! 种排列出现的可能性均相同)

const shuffle = arr => {
    for (let i = arr.length - 1; i > 0; i--) {
        let j = Math.floor(Math.random() * (i + 1));
        [arr[i], arr[j]] = [arr[j], arr[i]];
    }
};

class RandomBag {

    constructor() {
        this.data = [];
        this.count = 0;
    }

    isEmpty() {
        return this.count === 0;
    }

    size() {
        return this.count;
    }

    add(item) {
        this.data[this.count++] = item;
    }

    [Symbol.iterator]() {
        shuffle(this.data);
        const arr = this.data;
        const len = this.size();
        let index = 0;
        return {
            next() {
                return index < len ? { done: false, value: arr[index++] } : { done: true };
            }
        };
    }
}

const r = new RandomBag();
for (let i = 0; i < 5; i++) {
    r.add(i);
}

console.log(1, '--------');
for (let item of r) {
    console.log(item);
}
console.log(2, '========');
for (let item of r) {
    console.log(item);
}