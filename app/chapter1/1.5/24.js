// 适用于 Erdös - Renyi 模型的快速算法。
// 在练习1.5.23 的测试中增加加权 quick - union 算法和使用路径压缩的加权 quick - union 算法。
// 你能分辨出这两种算法的区别吗？

const _ = require('lodash');

const UF1 = require('./uf3'); // 加权quick union
const UF2 = require('./uf4'); // 使用路径压缩的加权quick union

const ErdosRenyi = (uf, N) => {
    const start = Date.now();
    while (uf.count() > 1) {
        const p = _.random(0, N - 1);
        const q = _.random(0, N - 1);
        if (uf.connected(p, q)) {
            continue;
        }
        uf.union(p, q);
    }
    return Date.now() - start;
};

const test = () => {
    const times = 100;
    for (let n = 2000; n < 1e5; n += n) {
        let time1 = 0, time2 = 0;
        for (let i = 0; i < times; i++) {
            const uf1 = new UF1(n, '加权quick union');
            const uf2 = new UF2(n, '基于路径压缩的加权quick union');
            time1 += ErdosRenyi(uf1, n);
            time2 += ErdosRenyi(uf2, n);
        }
        console.log('n = ', n, '加权quick union', time1 / times, '基于路径压缩的加权quick union', time2 / times, 'rate = ', time2 / time1);
    }
}

test();

// n = 2000 加权quick union 0.96 基于路径压缩的加权quick union 2.07 rate = 2.15625
// n = 4000 加权quick union 1.82 基于路径压缩的加权quick union 3.86 rate = 2.120879120879121
// n = 8000 加权quick union 3.69 基于路径压缩的加权quick union 8.17 rate = 2.214092140921409
// n = 16000 加权quick union 8.42 基于路径压缩的加权quick union 17.62 rate = 2.0926365795724466
// n = 32000 加权quick union 18.66 基于路径压缩的加权quick union 38.57 rate = 2.066988210075027
// n = 64000 加权quick union 44 基于路径压缩的加权quick union 86.4 rate = 1.9636363636363636