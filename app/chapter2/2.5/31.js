const T = 10;

const Ns = [1e3, 1e4, 1e5, 1e6];

const {randomArray} = require('../../util');

function doTest(M, N) {
  let sum = 0;
  for (let i = 0; i < T; i++) {
    let set = new Set();
    const arr = randomArray(0, M - 1, N);
    for (const item of arr) {
      set.add(item);
    }
    sum += set.size;
  }
  console.log('M = ', M, 'N = ', N, '实际值：', parseInt(sum / T), '期望值:', parseInt(M * (1 - Math.exp(-N / M))));
}

for (const n of Ns) {
  const Ms = [parseInt(n / 2), n, 2 * n];
  for (const m of Ms) {
    doTest(m, n);
  }
}

// M =  500 N =  1000 实际值： 434 期望值: 432
// M =  1000 N =  1000 实际值： 629 期望值: 632
// M =  2000 N =  1000 实际值： 787 期望值: 786
// M =  5000 N =  10000 实际值： 4312 期望值: 4323
// M =  10000 N =  10000 实际值： 6327 期望值: 6321
// M =  20000 N =  10000 实际值： 7863 期望值: 7869
// M =  50000 N =  100000 实际值： 43244 期望值: 43233
// M =  100000 N =  100000 实际值： 63210 期望值: 63212
// M =  200000 N =  100000 实际值： 78715 期望值: 78693
// M =  500000 N =  1000000 实际值： 432245 期望值: 432332
// M =  1000000 N =  1000000 实际值： 632239 期望值: 632120
// M =  2000000 N =  1000000 实际值： 786912 期望值: 786938
