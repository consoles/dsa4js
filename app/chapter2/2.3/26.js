// 子数组的大小

// 编写一个程序，在快排处理大小为N的数组的过程中，当子数组的大小小于M时排序方法需要切换为插入排序。将子数组的大小绘制成直方图。用N=10^5,M=10,20和50测试你的程序

const assert = require('assert');

const swap = require('../../swap');
const {shuffle, isSorted, genRandomDoubleArray} = require('../../util');
const {insertSortWithRange} = require('../../sort');

function _partition(arr, l, r) {
  const v = arr[l];
  let i = l, j = r + 1;
  while (true) {
    while (arr[++i] < v) if (i === r) break;
    while (arr[--j] > v) if (j === l) break;
    if (i >= j) break;
    swap(arr, i, j);
  }
  swap(arr, j, l);
  return j;
}

function _quickSort(arr, l, r, m) {
  if (l >= r) return;
  if (r - l + 1 <= m) {
    insertSortWithRange(arr, l, r);
    return;
  }
  const j = _partition(arr, l, r);
  _quickSort(arr, l, j - 1, m);
  _quickSort(arr, j + 1, r, m);
}

function quickSort(arr, m) {
  shuffle(arr);
  _quickSort(arr, 0, arr.length - 1, m);
  assert(isSorted(arr));
}

const mArr = [10, 20, 50];
const times = 10; // 运行10次，求平均值

const data = [];
for (const m of mArr) {
  const num = 1e5;
  let sum = 0;
  for (let i = 0; i < times; i++) {
    const arr = genRandomDoubleArray(num);
    const start = Date.now();
    quickSort(arr, m);
    sum += (Date.now() - start);
  }
  data.push(sum / times);
}

const seriesData = [{
  data,
  type: 'bar'
}];

require('fs').writeFileSync('./26.series.json', JSON.stringify(seriesData));
