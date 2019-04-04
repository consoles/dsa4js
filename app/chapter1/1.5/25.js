// 随机网格的倍率测试。
// 开发一个性能测试用例，从命令行接受一个 int 值 T 并进行 T 次以下实验：
// 使用练习 1.5.18 的用例生成一个 N×N 的随机网格，
// 所有连接的方向随机且排列随机。
// 和我们的开发用例一样使用 UnionFind 来检查触点的连通性，
// 不断循环直到所有触点均相互连通。
// 对于每个 N，打印出 N 值和平均所需的连接数以及前后两次运行时间的比值。
// 使用你的程序验证正文中的猜想：
// quick - find 和 quick - union 算法的运行时间是平方级别的，
// 加权 quick - union 算法则接近线性级别。
// 注意：
// 随着 N 值加倍，网格中触点的数量会乘以 4，
// 因此平方级别的算法运行时间会变为原来的 16 倍，
// 线性级别的算法的运行时间则变为原来的 4 倍。

const shuffle = arr => {
    for (let i = arr.length - 1; i > 0; i--) {
        let j = Math.floor(Math.random() * (i + 1));
        [arr[i], arr[j]] = [arr[j], arr[i]];
    }
};

class RandomBag {

    constructor() {
        this.data = [];
        this.count = 0;
    }

    isEmpty() {
        return this.count === 0;
    }

    size() {
        return this.count;
    }

    add(item) {
        this.data[this.count++] = item;
    }

    [Symbol.iterator]() {
        shuffle(this.data);
        const arr = this.data;
        const len = this.size();
        let index = 0;
        return {
            next() {
                return index < len ? { done: false, value: arr[index++] } : { done: true };
            }
        };
    }
}

class Connection {
    constructor(p, q) {
        this.p = p;
        this.q = q;
    }
}

class RandomGrid {
    static generate(N) {
        const bag = new RandomBag();
        // 横向连接
        let p = 0, q = 1;
        let i = 0;
        while (p < N * N && q < N * N) {
            if (q - p === 1) {
                // console.log(p, q);
                const conn = Math.random() > .5 ? new Connection(p, q) : new Connection(q, p);
                bag.add(conn);
            } else {
                break;
            }
            p++;
            q++;
            if (p === (i + 1) * N - 1) {
                p = ++i * N;
                q = p + 1;
            }
        }

        // 纵向连接
        p = 0, q = N;
        i = 0;
        while (p < N * N && q < N * N) {
            if (q - p === N) {
                const conn = Math.random() > .5 ? new Connection(p, q) : new Connection(q, p);
                bag.add(conn);
            } else {
                break;
            }
            p++;
            q++;
            if (p === (i + 1) * N) {
                p = ++i * N;
                q = (i + 1) * N;
            }
        }

        return bag;
    }
    static main(N) {
        return RandomGrid.generate(N);
    }
}

function check(conns, uf) {
    const start = Date.now();
    for (const conn of conns) {
        uf.union(conn.p, conn.q);
    }
    return Date.now() - start;
}

const UF1 = require('./uf1'); // quick find
const UF2 = require('./uf2'); // quick union
const UF3 = require('./uf3'); // 加权quick union

let lastCosts = [0, 0, 0];
const COUNT = 100;
const names = [[UF1, 'quick find'], [UF2, 'quick union'], [UF3, '加权quick union']];
for (let n = 10; n < 1e3; n += n) {
    const total = new Array(3).fill(0);
    for (let i = 0; i < COUNT; i++) {
        const conns = RandomGrid.main(n);
        const nowTimes = names.map(pair => check(conns, new pair[0](n * n, pair[1])));
        for (let i = 0; i < total.length; i++) {
            total[i] += nowTimes[i];
        }
    }
    nowCosts = total.map(x => x / COUNT);
    for (let i = 0; i < nowCosts.length; i++) {
        console.log('n = ', n, names[i][1], 'took', nowCosts[i], 'rate = ', nowCosts[i] / lastCosts[i]);
    }
    console.log();
    lastCosts = nowCosts;
}

// n =  10 quick find took 0.4 rate =  Infinity
// n =  10 quick union took 0.18 rate =  Infinity
// n =  10 加权quick union took 0.12 rate =  Infinity

// n =  20 quick find took 0.9 rate =  2.25
// n =  20 quick union took 0.25 rate =  1.3888888888888888
// n =  20 加权quick union took 0.21 rate =  1.75

// n =  40 quick find took 11.89 rate =  13.211111111111112
// n =  40 quick union took 1.25 rate =  5
// n =  40 加权quick union took 0.52 rate =  2.4761904761904763

// n =  80 quick find took 186.88 rate =  15.717409587888982
// n =  80 quick union took 13.18 rate =  10.544
// n =  80 加权quick union took 2.49 rate =  4.788461538461538

// n =  160 quick find took 2991.74 rate =  16.00888270547945
// n =  160 quick union took 277.38 rate =  21.045523520485585
// n =  160 加权quick union took 20.58 rate =  8.265060240963853

// n =  320 quick find took 57642.26 rate =  19.267135513112773
// n =  320 quick union took 8549.92 rate =  30.823851755714184
// n =  320 加权quick union took 94.27 rate =  4.580660835762877

// n =  640 quick find took 895841.73 rate =  15.541405385562605
// n =  640 quick union took 326304.55 rate =  38.16463194977263
// n =  640 加权quick union took 846.87 rate =  8.983451787419115