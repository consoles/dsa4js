// 使用路径压缩的 quick-union 算法。
// 根据路径压缩修改 quick-union 算法（请见 1.5.2.3 节），在 find() 方法中添加一个循环来将从 p 到根节点的路径上的每个触点都连接到根节点。
// 给出一列输入，使该方法能够产生一条长度为 4 的路径。
// 注意：该算法的所有操作的均摊成本已知为对数级别。

const UF2 = require('./uf2');

class UF extends UF2 {

    // 在find中实现路径压缩
    find(p) {
        const root = super.find(p);
        while (p !== this.ids[p]) {
            const parent = this.ids[p];
            this.ids[p] = root;
            p = parent;
        }
        return root;
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

// for (const file of [tinyUF, mediumUF, largeUF]) {
//     const ret = test(file, UF, '路径压缩的quick union');
//     console.log(ret);
// }

// { name: '路径压缩的quick union', n: 10, count: 2, elapsed: 1 }
// { name: '路径压缩的quick union', n: 625, count: 3, elapsed: 1 }
// { name: '路径压缩的quick union', n: 1000000, count: 6, elapsed: 714 }

// 最坏的情形：(0,1),(1,2),(2,3),(3,4)
const uf = new UF(5);
for (let i = 0; i < 4; i++) {
    uf.union(i, i + 1);
}
console.log(uf.ids);