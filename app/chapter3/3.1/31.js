const {genRandomStringArr} = require('../../util'); // 太长的字符串数组会内存溢出

/**
 * 符号表测试
 * @param ST 符号表类对象
 * @param n 符号表中键值对的数量
 * @param averageHit 平均命中数
 */
function stPerformanceTest(ST, n, averageHit) {
  const testCount = 10;

  let time = 0;
  for (let i = 0; i < testCount; i++) {
    const keys = genRandomStringArr(n, 2, 50);
    const keysNotExists = genRandomStringArr(n, 51, 60);

    const start = Date.now();
    const st = new ST();
    // put
    for (let i = 0; i < keys.length; i++) {
      st.put(keys[i], i);
    }
    // get
    for (let i = 0; i < n; i++) {
      for (let j = 0; j < averageHit; j++) {
        st.get(keys[i]); // 命中查询
        st.get(keysNotExists[i]); // 非命中查询
      }
    }

    time += (Date.now() - start);
  }

  return {
    name: ST.name,
    elapsed: time / testCount
  };
}

const BinarySearchST = require('../BinarySearchST');
const SequentialSearchST = require('../SequentialSearchST');

for (let n = 1e3; n <= 1e6; n *= 10) {
  for (const ST of [BinarySearchST, SequentialSearchST]) {
    const ret = stPerformanceTest(ST, n, 10);
    console.log('n = ', n, JSON.stringify(ret));
  }
}

// n =  1000 {"name":"BinarySearchST","elapsed":12.7}
// n =  1000 {"name":"SequentialSearchST","elapsed":95.7}
// n =  10000 {"name":"BinarySearchST","elapsed":211.3}
// n =  10000 {"name":"SequentialSearchST","elapsed":7072.2}
// n =  100000 {"name":"BinarySearchST","elapsed":10722.9}
// n =  100000 {"name":"SequentialSearchST","elapsed":2063085.1}
