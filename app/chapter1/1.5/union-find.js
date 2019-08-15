const fs = require('fs');
const path = require('path');

const tinyUF = '../../../test/input/algs4-data/tinyUF.txt';
const mediumUF = '../../../test/input/algs4-data/mediumUF.txt';
const largeUF = '../../../test/input/algs4-data/largeUF.txt';

const util = require('../../../app/util');
const UF1 = require('./uf1');
const UF2 = require('./uf2');
const UF3 = require('./uf3');
const UF4 = require('./uf4');

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
  };
};

// { name: 'quick find', n: 10, count: 2, elapsed: 0 }
// { name: 'quick find', n: 625, count: 3, elapsed: 10 }
// { name: 'quick find', n: 1000000, count: 6, elapsed: 3833606 }

// { name: 'quick union', n: 10, count: 2, elapsed: 0 }
// { name: 'quick union', n: 625, count: 3, elapsed: 6 }
// { name: 'quick union', n: 1000000, count: 6, elapsed: 15300940 }

// { name: '加权quick union', n: 10, count: 2, elapsed: 0 }
// { name: '加权quick union', n: 625, count: 3, elapsed: 0 }
// { name: '加权quick union', n: 1000000, count: 6, elapsed: 256 }

// { name: '路径压缩 & 加权quick union', n: 10, count: 2, elapsed: 0 }
// { name: '路径压缩 & 加权quick union', n: 625, count: 3, elapsed: 2 }
// { name: '路径压缩 & 加权quick union', n: 1000000, count: 6,elapsed: 688}

// 成本模型是数组元素的访问次数（无论读写）
for (const file of [tinyUF, mediumUF, largeUF]) {
  // const ret = test(file, UF1,'quick find');
  // const ret = test(file, UF2,'quick unoin');
  // const ret = test(file, UF3, '加权quick union');
  // const ret = test(file, UF4, '路径压缩 & 加权quick union');
  // console.log(ret);
}

function doTest(ints, uf) {
  let i = 1;
  let count = 0;
  const currentConnectedPoints = [];
  const currentUnionPoints = [];
  const totalAccessPoints = [];
  while (true) {
    if (i >= ints.length) {
      break;
    }
    const p = ints[i++];
    const q = ints[i++];
    let x = count++;
    const connected = uf.connected(p, q);
    currentConnectedPoints.push([x, uf.currentAccessArrayCount]);
    if (connected) {
      continue;
    }
    uf.union(p, q);
    currentUnionPoints.push([x, uf.currentAccessArrayCount]);
    totalAccessPoints.push([x, uf.totalAccessArrayCount / count]);
  }
  return {
    currentConnectedPoints,
    currentUnionPoints,
    totalAccessPoints
  };
}

const buildEchartsRenderData = () => {
  const ints = util.readInts(mediumUF);
  const N = ints[0];
  const data = [];
  for (const [UF, desc] of [[UF1, 'quick find'], [UF2, 'quick unoin'], [UF3, '加权quick unoin'], [UF4, '路径压缩的加权quick union']]) {
    const ret = doTest(ints, new UF(N, desc));
    data.push({desc, ret});
  }

  return data;
};

const renderData = buildEchartsRenderData();

fs.writeFileSync(path.join(__dirname, 'union-find.json'), JSON.stringify(renderData));

// quick union的优化：union的时候总是将较小的根节点指向较大的根节点（或者将较大的根节点指向较小的根节点，反正是单一方向），这样也可以有效降低树的深度(还有一种有效的加权，基于树的深度，参见练习1.5.14);在find的时候将所有路径上的节点直接指向根节点（路径压缩）；这两种方法都能很大程度上提升性能。
