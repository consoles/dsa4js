// 为 StaticSETofInts （请见表 1.2.15） 添加一个实例方法 howMany() ，
// 找出给定键的出现次数且在最坏情况下所需的运行时间应该和 log(N) 成正比。

class StaticSETofInts {
    constructor(keys) {
        const arr = keys.slice();
        arr.sort((a, b) => a - b);
        // for (let i = 1; i < arr.length; i++) {
        //     if (arr[i] === arr[i - 1]) {
        //         throw new Error('元素重复');
        //     }
        // }
        this.arr = arr;
    }
    howMany(key) {
        // const index = this.rank(key);
        // let cnt = 0;
        // if (index === -1) return cnt;
        // let i = index;
        // cnt = 1;
        // // 向左
        // while (this.arr[i - 1] === key) {
        //     i--;
        //     cnt++;
        // }
        // // 向右
        // i = index;
        // while (this.arr[i + 1] === key) {
        //     i++;
        //     cnt++;
        // }
        // return cnt;
        return this._howMany(key, 0, this.arr.length - 1);
    }
    _howMany(key, lo, hi) {
        const index = this.rankWithRange(key, lo, hi);
        return index === -1 ? 0 : 1 + this._howMany(key, lo, index - 1) + this._howMany(key, index + 1, hi);
    }
    contains(key) {
        return this.rank(key) !== -1;
    }
    rank(key) {
        return this.rankWithRange(key, 0, this.arr.length - 1);
    }
    rankWithRange(key, lo, hi) {
        while (lo <= hi) {
            const mid = lo + Math.floor((hi - lo) / 2);
            const item = this.arr[mid];
            if (item === key) {
                return mid;
            }
            if (key < item) {
                hi = mid - 1;
            } else {
                lo = mid + 1;
            }
        }
        return -1;
    }
}

const arr = [1, 1, 1, 2, 3, 3, 3, 4, 5, 5, 6, 7, 8, 9];
const set = new StaticSETofInts(arr);
const ret = set.howMany(1);
console.log(ret);
debugger