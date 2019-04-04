// 动画。
// 编写一个 RandomGrid（请见练习 1.5.18）的用例，
// 和我们开发用例一样使用 UnionFind 来检查触点的连通性并在处理时用 StdDraw 将它们绘出。

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
        const bag = RandomGrid.generate(N);
        const arr = [];
        for (const item of bag) {
            console.log(item);
            arr.push(item);
        }
        return arr;
    }
}

const express = require('express');
const ejs = require('ejs');

const renderData = (() => {
    const UF = require('./uf3');
    const N = 5;
    const conns = RandomGrid.main(N);
    // 根绝连接得到所有的点
    const points = [];
    const lines = [];
    const uf = new UF(N * N);
    // 升维（一维转二维）
    for (const { p, q } of conns) {
        const x1 = p / N | 0;
        const y1 = p % N;
        const x2 = q / N | 0;
        const y2 = q % N;
        points.push([x1, y1]);
        points.push([x2, y2]);
        if (!uf.connected(p, q)) {
            uf.union(p, q);
            lines.push([[x1, y1], [x2, y2]]);
        }
    }
    return {
        lines, points
    };
})();

var app = express();

app.engine('html', ejs.__express);
app.set('view engine', 'html');
app.set('views', './');

app.get('/', function (req, res) {
    res.render('19');
});
app.post('/', function (req, res) {
    res.json(renderData);
});
app.listen(3000, function () {
    console.log('express listening on port 3000!');
});