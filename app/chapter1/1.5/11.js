// 实现加权 quick-find 算法，其中我们总是将较小的分量重命名为较大分量的标识符。
// 这种改变会对性能产生怎样的影响？

// 类似于加权 quick-union 的做法，新增一个 size[] 数组以记录各个根节点的大小。
// 每次合并时先比较一下两棵树的大小，再进行合并。
// 这样会略微减少赋值语句的执行次数，提升性能

class UF {
    constructor(N, desc) {
        const ids = [];
        for (let i = 0; i < N; i++) {
            ids.push(i);
        }
        this.ids = ids;
        this.size = new Array(N).fill(1);
        this.componentCount = N;
        this.desc = desc;
    }
    union(p, q) {
        const pID = this.find(p);
        const qID = this.find(q);
        if (pID === qID) return;

        const baseID = pID > qID ? pID : qID;
        const cmpID = baseID === pID ? qID : pID;

        for (let i = 0; i < this.ids.length; i++) {
            if (this.ids[i] === cmpID) {
                this.ids[i] = baseID;
            }
        }
        this.componentCount--;
    }
    find(p) {
        return this.ids[p];
    }
    connected(p, q) {
        return this.find(p) === this.find(q);
    }
    count() {
        return this.componentCount;
    }
}

const tinyUF = '../../../test/input/algs4-data/tinyUF.txt';
const mediumUF = '../../../test/input/algs4-data/mediumUF.txt';
const largeUF = '../../../test/input/algs4-data/largeUF.txt';

const util = require('../../util');

const test = (filename, UF, desc) => {
    const ints = util.readInts(filename);
    const N = ints[0];
    const start = Date.now();
    const uf = new UF(N, desc);
    let i = 1;
    while (true) {
        if (i >= ints.length) {
            break;
        }
        const p = ints[i++];
        const q = ints[i++];
        if (uf.connected(p, q)) {
            continue;
        }
        uf.union(p, q);
    }
    const count = uf.count();
    const end = Date.now();
    return {
        name: uf.desc,
        n: N,
        count,
        elapsed: end - start
    }
}

for (const file of [tinyUF, mediumUF, largeUF]) {
    const ret = test(file, UF, '优化的quick find');
    console.log(ret);
}

// { name: '优化的quick find', n: 10, count: 2, elapsed: 0 }
// { name: '优化的quick find', n: 625, count: 3, elapsed: 8 }
// { name: '优化的quick find', n: 1000000, count: 6, elapsed: 3128496 }