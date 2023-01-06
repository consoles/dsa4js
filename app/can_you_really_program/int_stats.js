// 设计一个 IntStats 类，对一个整数列表进行汇总和统计，提供 3 个共有方法
// insert(n): 向列表中添加一个整数，插入顺序不限
// getAverage(): 返回到目前为止插入整数的算术平均值
// getMedian(): 返回到目前为止插入整数的中位数

// 最朴素实现
class IntStats1 {
    constructor() {
        this.list = []
    }
    // O(1)
    insert(n) {
        this.list.push(n)
    }
    // O(N)
    get average() {
        let sum = 0
        for (const n of this.list) {
            sum += n
        }
        return sum / this.list.length
    }
    // O(N*lgN)
    get median() {
        if (this.list.length === 0) return null
        this.list.sort((a, b) => a - b)
        const idx = this.list.length / 2
        if (idx % 2 === 0) {
            return (this.list[idx - 1] + this.list[idx]) / 2
        }
        return this.list[Number.parseInt(idx)]
    }
}

const array = [2, 10, 11, 20]
// const array = [2, 10, 11, 20, 100]

let stats = new IntStats1()
for (const n of array) {
    stats.insert(n)
}
console.log('average:', stats.average);
console.log('median:', stats.median);

// 快速插入：常数时间 insert 和 getAverage
class IntStats2 {
    constructor() {
        this.list = []
        this.sum = 0
    }
    // O(1)
    insert(n) {
        this.list.push(n)
        this.sum += n
    }
    // O(1)
    get average() {
        return this.sum / this.list.length
    }
    // O(N*lgN)
    get median() {
        if (this.list.length === 0) return null
        this.list.sort((a, b) => a - b)
        const idx = this.list.length / 2
        if (idx % 2 === 0) { // 偶数，其实用 this.list.length % 2 === 0 判断更好理解
            return (this.list[idx - 1] + this.list[idx]) / 2
        }
        return this.list[Number.parseInt(idx)]
    }
}

stats = new IntStats2()
for (const n of array) {
    stats.insert(n)
}
console.log('average:', stats.average);
console.log('median:', stats.median);

// 快速查询：常数时间 getMedian 和 getAverage
class IntStats3 {
    constructor() {
        this.list = []
        this.sum = 0
    }
    // O(N*lgN)
    insert(n) {
        this.list.push(n)
        this.sum += n
        // 每次插入后对列表进行排序，可以很容易将计算负载从 getMedian 转移到 insert
        // 插入方法将从 O(1) 增加到 O(N*logN)
        this.list.sort((a, b) => a - b)
    }
    // O(1)
    get average() {
        return this.sum / this.list.length
    }
    // O(1)
    get median() {
        if (this.list.length === 0) return null
        const idx = this.list.length / 2
        if (idx % 2 === 0) { // 偶数，其实用 this.list.length % 2 === 0 判断更好理解
            return (this.list[idx - 1] + this.list[idx]) / 2
        }
        return this.list[Number.parseInt(idx)]
    }
}

stats = new IntStats3()
for (const n of array) {
    stats.insert(n)
}
console.log('average:', stats.average);
console.log('median:', stats.median);

// 快速查询：常数时间 getMedian 和 getAverage
class IntStats4 {
    constructor() {
        this.list = []
        this.sum = 0
    }
    // O(N)
    insert(n) {
        // IntStats3 的优化，每次插入元素的时候放置到正确的位置
        let i = 0
        for(const num of this.list) {
            if (num >= n) {
                break
            }
            i++
        }
        this.list[i] = n;
        this.sum += n
    }
    // O(1)
    get average() {
        return this.sum / this.list.length
    }
    // O(1)
    get median() {
        if (this.list.length === 0) return null
        const idx = this.list.length / 2
        if (idx % 2 === 0) { 
            return (this.list[idx - 1] + this.list[idx]) / 2
        }
        return this.list[Number.parseInt(idx)]
    }
}

stats = new IntStats4()
for (const n of array) {
    stats.insert(n)
}
console.log('average:', stats.average);
console.log('median:', stats.median);

// 不可能有一种实现保证 getMedian 和 insert 都是线性时间。
// 如果存在这样的实现就意味着能在O(N)时间排序任意数据！
