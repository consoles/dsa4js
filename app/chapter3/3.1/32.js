const StopWatch = require('../StopWatch');

function stPerformanceTest(ST, keys, name) {
  const sw = new StopWatch();
  const st = new ST();
  sw.start(name + '$' + ST.name);
  for (let i = 0; i < keys.length; i++) {
    st.put(keys[i], i);
  }
  sw.stopAndPrint();
}

const BinarySearchST = require('../BinarySearchST');
const SequentialSearchST = require('../SequentialSearchST');

const _ = require('lodash');

const n = 10000;
const keys = [];
for (let i = 0; i < n; i++) {
  keys.push(i);
}
let twoElements = [];
for (let i = 0; i < n; i++) {
  twoElements.push(i % 2);
}
twoElements = _.shuffle(twoElements);

const data = [
  [keys, '有序键列'],
  [keys.reverse(), '逆序键列'],
  [new Array(n).fill(1), '键全部相同的'],
  [twoElements, '只含有2种元素']
];

for (const d of data) {
  for (const ST of [BinarySearchST, SequentialSearchST]) {
    stPerformanceTest(ST, d[0], d[1]);
  }
}

// 有序键列$BinarySearchST  =>  1125
// 有序键列$SequentialSearchST  =>  133
// 逆序键列$BinarySearchST  =>  1188
// 逆序键列$SequentialSearchST  =>  147
// 键全部相同的$BinarySearchST  =>  5
// 键全部相同的$SequentialSearchST  =>  1
// 只含有2种元素$BinarySearchST  =>  10
// 只含有2种元素$SequentialSearchST  =>  0

// 2种数据结构没有特别大的性能差距，性能严重依赖输入
