const BST = require('../BST');
const {randomArray} = require('../../util');

function test(bounds) {
  const nums = randomArray(0, bounds, 1e6);
  const bst = new BST();
  let putTime = 0;
  let getTime = 0;

  for (const num of nums) {
    if (bst.contains(num)) {
      const startGet = Date.now();
      const lastCount = bst.get(num);
      const endGet = Date.now();

      const startPut = Date.now();
      bst.put(num, lastCount + 1);
      const endPut = Date.now();

      putTime += (endPut - startPut);
      getTime += (endGet - startGet);
    } else {
      const startPut = Date.now();
      bst.put(num, 1);
      const endPut = Date.now();
      putTime += (endPut - startPut);
    }
  }

  console.log('1e6 arrays', `[0,${bounds}]`, 'putTime', putTime, 'getTime', getTime, 'put / get', putTime / getTime);
}

for (let num = 1e2; num <= 1e8; num *= 10) {
  test(num);
}

// 1e6 arrays [0,100] putTime 188 getTime 118 put / get 1.5932203389830508
// 1e6 arrays [0,1000] putTime 283 getTime 180 put / get 1.5722222222222222
// 1e6 arrays [0,10000] putTime 431 getTime 238 put / get 1.8109243697478992
// 1e6 arrays [0,100000] putTime 628 getTime 230 put / get 2.7304347826086954
// 1e6 arrays [0,1000000] putTime 876 getTime 117 put / get 7.487179487179487
// 1e6 arrays [0,10000000] putTime 969 getTime 11 put / get 88.0909090909091
// 1e6 arrays [0,100000000] putTime 952 getTime 3 put / get 317.3333333333333

// 重复元素越多，put和get越接近，因为大部分命中了contains那个分之，如果数组中几乎没有重复元素则put操作占用较多时间
