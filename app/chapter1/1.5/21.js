// Erdös - Renyi 模型。
// 使用练习 1.5.17 的用例验证这个猜想：
// 得到单个连通分量所需生成的整数对数量为 ~1 / 2NlnN。

const _ = require('lodash');

const UF = require('./uf3');

const ErdosRenyi = N => {
    const uf = new UF(N);
    let count = 0;
    while (uf.count() > 1) {
        count++;
        const p = _.random(0, N - 1);
        const q = _.random(0, N - 1);
        if (uf.connected(p, q)) {
            continue;
        }
        // console.log('union', p, q);
        uf.union(p, q);
    }
    return count;
};

for (let n = 10; n < 1e4; n *= 2) {
    let total = 0;
    for (let i = 0; i < 1000; i++) {
        total += ErdosRenyi(n);
    }
    console.log('实验结果', total / 1000, '1/2NlnN = ', 0.5 * n * Math.log2(n));
}

// 实验结果 16.767 1 / 2NlnN = 16.609640474436812
// 实验结果 38.322 1 / 2NlnN = 43.219280948873624
// 实验结果 88.001 1 / 2NlnN = 106.43856189774725
// 实验结果 200.472 1 / 2NlnN = 252.8771237954945
// 实验结果 450.168 1 / 2NlnN = 585.754247590989
// 实验结果 1021.08 1 / 2NlnN = 1331.5084951819779
// 实验结果 2254.347 1 / 2NlnN = 2983.0169903639558
// 实验结果 4970.118 1 / 2NlnN = 6606.0339807279115
// 实验结果 10723.656 1 / 2NlnN = 14492.067961455823
// 实验结果 23354.453 1 / 2NlnN = 31544.135922911646