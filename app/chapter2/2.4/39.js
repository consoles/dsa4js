const swap = require('../../swap');

function sink(arr, k, n) {
  while (k < n) {
    let j = 2 * k + 1;
    if (j >= n) break;
    if (j + 1 < n && arr[j + 1] > arr[j]) j++;
    if (arr[k] < arr[j]) {
      swap(arr, k, j);
      k = j;
    } else {
      break;
    }
  }
}

function heapSort(arr) {
  // 从底向上建最大堆(第一个非叶子节点)
  const start = Date.now();
  let n = arr.length;
  for (let k = parseInt(n / 2); k >= 0; k--) {
    sink(arr, k, n);
  }
  const afterBuildHeap = Date.now();
  while (n > 0) {
    swap(arr, 0, --n);
    sink(arr, 0, n);
  }
  const afterSort = Date.now();
  return {
    buildHeap: afterBuildHeap - start,
    sink: afterSort - afterBuildHeap
  };
}

const ns = [1e3, 1e6, 1e7];
const genRandomDoubleArray = require('../../util').genRandomDoubleArray;
const testCount = 10;

for (const n of ns) {
  let buildHeap = 0;
  let sink = 0;
  for (let i = 0; i < testCount; i++) {
    const arr = genRandomDoubleArray(n);
    const ret = heapSort(arr);
    buildHeap += ret.buildHeap;
    sink += ret.sink;
  }
  console.log('n = ', n, '建堆阶段', buildHeap / testCount, '下沉阶段', sink / testCount);
}

// n =  1000 建堆阶段 0.1 下沉阶段 0.5
// n =  1000000 建堆阶段 13.1 下沉阶段 173.1
// n =  10000000 建堆阶段 135.2 下沉阶段 2723.4
// js创建1e8长度的数组会爆栈
