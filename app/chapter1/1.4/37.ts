// 自动装箱的性能代价。通过实验在你的计算机上计算使用自动装箱所付出的性能代价。
// 实现一个 FixedCapacityStackOfInts，并使用类似 DoublingRatio 的用例比较它和泛型 FixedCapacityStack 在进行大量 push() 和 pop() 时的性能。

// ts-node 37.ts

// 整数
class FixedCapacityStackOfInts {
    private capacity: number;
    private data: number[];
    private size: number = 0;
    constructor(capacity: number) {
        this.capacity = capacity;
        this.data = new Array(capacity);
    }
    isFull() {
        return this.size === this.capacity;
    }
    isEmpty() {
        return this.size === 0;
    }
    push(item: number) {
        if (this.isFull()) {
            return false;
        }
        this.data[this.size++] = item;
    }
    pop(): number {
        if (this.isEmpty()) {
            return null;
        }
        return this.data[--this.size];
    }
}

// 泛型
class FixedCapacityStack<T> {
    private capacity: number;
    private data: T[];
    private size: number = 0;
    constructor(capacity: number) {
        this.capacity = capacity;
        this.data = new Array(capacity);
    }
    isFull() {
        return this.size === this.capacity;
    }
    isEmpty() {
        return this.size === 0;
    }
    push(item: T) {
        if (this.isFull()) {
            return false;
        }
        this.data[this.size++] = item;
    }
    pop(): T {
        if (this.isEmpty()) {
            return null;
        }
        return this.data[--this.size];
    }
}

const MAX = 1e8;
import * as _ from "lodash";
const doubleTest = n => {
    const arr = [];
    for (let i = 0; i < n; i++) {
        arr.push(_.random(-MAX, MAX));
    }
    const intStack = new FixedCapacityStackOfInts(n);
    const genericStack = new FixedCapacityStack<number>(n);
    const start = Date.now();
    for (let i = 0; i < n; i++) {
        intStack.push(arr[i]);
    }
    while (!intStack.isEmpty()) {
        intStack.pop();
    }
    const time1 = Date.now();
    for (let i = 0; i < n; i++) {
        genericStack.push(arr[i]);
    }
    while (!intStack.isEmpty()) {
        genericStack.pop();
    }
    const time2 = Date.now();
    return {
        'times': n,
        'int': time1 - start,
        'generic': time2 - time1
    }
}

let n = 100;
while (true) {
    const ret = doubleTest(n);
    console.log(ret);
    n += n;
}

// { times: 100, int: 0, generic: 0 }
// { times: 200, int: 0, generic: 0 }
// { times: 400, int: 0, generic: 0 }
// { times: 800, int: 0, generic: 0 }
// { times: 1600, int: 1, generic: 0 }
// { times: 3200, int: 2, generic: 0 }
// { times: 6400, int: 0, generic: 0 }
// { times: 12800, int: 0, generic: 0 }
// { times: 25600, int: 1, generic: 0 }
// { times: 51200, int: 0, generic: 0 }
// { times: 102400, int: 1, generic: 0 }
// { times: 204800, int: 1, generic: 1 }
// { times: 409600, int: 3, generic: 1 }
// { times: 819200, int: 4, generic: 3 }
// { times: 1638400, int: 11, generic: 6 }
// { times: 3276800, int: 18, generic: 8 }
// { times: 6553600, int: 34, generic: 19 }
// { times: 13107200, int: 79, generic: 49 }
// { times: 26214400, int: 150, generic: 92 }

// ts中泛型反而更快，其他静态语言中泛型应该比不上预定义数据类型的。