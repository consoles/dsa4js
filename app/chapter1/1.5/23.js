// 在 Erdös - Renyi 模型下比较 quick - find 算法和 quick - union 算法。
// 开发一个性能测试用例，
// 从命令行接受一个 int 值 T 并进行 T 次以下实验：
// 使用练习 1.5.17 的用例生成随机连接。
// 保存这些连接并和我们的开发用例一样分别用 quick - find 和 quick - union 算法检查触点的连通性，
// 不断循环直到所有触点均相互连通。
// 对于每个 N，打印出 N 值和两种算法的运行时间比值。

const _ = require('lodash');

const UF1 = require('./uf1'); // quick find
const UF2 = require('./uf2'); // quick unoin

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
            const uf1 = new UF1(n, 'quick find');
            const uf2 = new UF2(n, 'quick union');
            time1 += ErdosRenyi(uf1, n);
            time2 += ErdosRenyi(uf2, n);
        }
        console.log('n = ', n, 'quick find', time1 / times, 'quick uinon', time2 / times, 'rate = ', time2 / time1);
    }
}

test();

// n = 2000 quick find 22.08 quick uinon 15.04 rate = 0.6811594202898551
// n = 4000 quick find 81.42 quick uinon 59.81 rate = 0.7345860967821174
// n = 8000 quick find 319.92 quick uinon 315.2 rate = 0.9852463115778944
// n = 16000 quick find 1292.24 quick uinon 1813.37 rate = 1.4032764811490126
// n = 32000 quick find 5260.88 quick uinon 10091.89 rate = 1.9182893356244584
// n = 64000 quick find 24195.03 quick uinon 79524.18 rate = 3.2867981564808972