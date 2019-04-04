// Erdös - Renyi 模型的均摊成本图像。
// 开发一个用例，从命令行接受一个 int 值 N，
// 在 0 到 N - 1 之间产生随机整数对，
// 调用 connected() 判断它们是否相连，
// 如果不是则用 union() 方法（和我们的开发用例一样）。
// 不断循环直到所有触点互通。
// 按照正文的样式将所有操作的均摊成本绘制成图像。

const express = require('express');
const ejs = require('ejs');

const UF1 = require('./uf1');
const UF2 = require('./uf2');
const UF3 = require('./uf3');
const UF4 = require('./uf4');

const items = [
    ['quick find', UF1],
    ['quick union', UF2],
    ['加权quick union', UF3],
    ['路径压缩的加权quick union', UF4]
];

const _ = require('lodash');

function doTest(n) {
    const data = {};
    for (let s = 1; s < n; s++) {
        for (let i = 0; i < items.length; i++) {
            const [desc, clazz] = items[i];
            const uf = new clazz(n, desc);
            while (uf.count() > 1) {
                const p = _.random(0, n - 1);
                const q = _.random(0, n - 1);
                if (!uf.connected(p, q)) {
                    uf.union(p, q);
                }
            }
            if (!data[desc]) {
                data[desc] = [];
            }
            data[desc].push([s, uf.totalAccessArrayCount]);
        }
    }
    return data;
}

const app = express();

app.engine('html', ejs.__express);
app.set('view engine', 'html');
app.set('views', './');

app.get('/', function (req, res) {
    res.render('26');
});
app.get('/data', function (req, res) {
    const n = parseInt(req.query.n);
    const data = doTest(n);
    res.json(data);
});
app.listen(3000, function () {
    console.log('express listening on port 3000!');
});