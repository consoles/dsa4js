// Erdös - Renyi 的倍率实验。
// 开发一个性能测试用例，从命令行接受一个 int 值 T 并进行 T 次以下实验：
// 使用练习 1.5.17 的用例生成随机连接，
// 和我们的开发用例一样使用 UnionFind 来检查触点的连通性，
// 不断循环直到所有触点都相互连通。
// 对于每个 N，
// 打印出 N 值和平均所需的连接数以及前后两次运行时间的比值。
// 使用你的程序验证正文中的猜想：
// quick - find 算法和 quick - union 算法的运行时间是平方级别的，
// 加权 quick - union 算法则接近线性级别。

const _ = require('lodash');

const UF1 = require('./uf1'); // quick find
const UF2 = require('./uf2'); // quick union
const UF3 = require('./uf3'); // 加权quick union

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
    let lastCostTimes = [0, 0, 0];
    for (let n = 2e3; n < 1e5; n += n) {
        const ufs = [new UF1(n, 'quick find'), new UF2(n, 'quick union'), new UF3(n, 'weighted quick union')];
        const nowCostTimes = ufs.map(x => ErdosRenyi(x, n));
        for (let i = 0; i < ufs.length; i++) {
            const uf = ufs[i];
            console.log('n = ', n, uf.desc, 'cost = ', nowCostTimes[i], 'rate = ', nowCostTimes[i] / lastCostTimes[i]);
        }
        console.log();
        lastCostTimes = nowCostTimes;
    }
}

test();

// n = 2000 quick find cost = 24 rate = Infinity
// n = 2000 quick union cost = 22 rate = Infinity
// n = 2000 weighted quick union cost = 10 rate = Infinity

// n = 4000 quick find cost = 47 rate = 1.9583333333333333
// n = 4000 quick union cost = 36 rate = 1.6363636363636365
// n = 4000 weighted quick union cost = 5 rate = 0.5

// n = 8000 quick find cost = 185 rate = 3.9361702127659575
// n = 8000 quick union cost = 211 rate = 5.861111111111111
// n = 8000 weighted quick union cost = 5 rate = 1

// n = 16000 quick find cost = 742 rate = 4.010810810810811
// n = 16000 quick union cost = 817 rate = 3.8720379146919433
// n = 16000 weighted quick union cost = 10 rate = 2

// n = 32000 quick find cost = 2982 rate = 4.018867924528302
// n = 32000 quick union cost = 4359 rate = 5.335373317013464
// n = 32000 weighted quick union cost = 25 rate = 2.5

// n = 64000 quick find cost = 12672 rate = 4.249496981891348
// n = 64000 quick union cost = 37383 rate = 8.57604955264969
// n = 64000 weighted quick union cost = 53 rate = 2.12