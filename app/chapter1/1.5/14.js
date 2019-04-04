// 根据高度加权的 quick-union 算法。
// 给出 UF 的一个实现，使用和加权 quick-union 算法相同的策略，但记录的是树的高度并总是将较矮的树连接到较高的树上。
// 用算法证明 N 个触点的树的高度不会超过其大小的对数级别。

const UF2 = require('./uf2');

class UF extends UF2 {
    constructor(N, desc) {
        super(N, desc);
        this.height = new Array(N).fill(0);
    }
    /**
     * 合并两个高度相等的树会导致树的高度加1
     */
    union(p, q) {
        const pRoot = this.find(p);
        const qRoot = this.find(q);
        if (pRoot === qRoot) return;

        if (this.height[pRoot] > this.height[qRoot]) {
            this.ids[qRoot] = pRoot;
        } else if (this.height[pRoot] < this.height[qRoot]) {
            this.ids[pRoot] = qRoot;
        } else {
            this.ids[qRoot] = pRoot;
            this.height[pRoot]++;
        }
        this.componentCount--;
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
    const ret = test(file, UF, '基于树高度加权的quick union');
    console.log(ret);
}

// { name: '基于树高度加权的quick union', n: 10, count: 2, elapsed: 0 }
// { name: '基于树高度加权的quick union', n: 625, count: 3, elapsed: 2 }
// { name: '基于树高度加权的quick union', n: 1000000, count: 6, elapsed: 264 }

// 一次 Union 操作只可能发生如下两种情况。
// 1.两棵树的高度相同，这样合并后的新树的高度等于较大那棵树的高度 + 1。
// 2.两棵树的高度不同，这样合并后的新树高度等于较大那棵树的高度。

// 现在证明通过加权 quick - union 算法构造的高度为 h 的树至少包含 2^h 个结点。
// 基础情况，高度 h = 0, 结点数 k = 1。
// 为了使高度增加，必须用一棵高度相同的树合并，而 h = 0 时结点数一定是 1，则：
// h = 1, k = 2
// 由于两棵大小不同的树合并，最大高度不会增加，只会增加结点数。
// 因此，每次都使用相同高度的最小树进行合并，有：
// h = 2, k = 4
// h = 3, k = 8
// h = 4, k = 16
// ......
// 递推即可得到结论，k ≥ 2^h
// 因此 h <= lgk