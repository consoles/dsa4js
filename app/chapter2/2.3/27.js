// 忽略小数组
// 用实验对比以下处理小数组的方法和练习2.3.25的处理方法的效果：在快排中直接忽略小数组，仅在快排结束后运行一次插入排序。注意：可以通过这些实验估计出电脑缓存的大小，因为当数组大小超过缓存时这种方法方法的性能可能下降

const assert = require('assert');

const swap = require('../../swap');
const {shuffle, isSorted, randomDoubleArray} = require('../../util');
const {insertSort} = require('../../sort');

function partition(arr, l, r) {
  const v = arr[l];
  let i = l, j = r + 1;
  while (true) {
    while (arr[++i] < v) if (i === r) break;
    while (arr[--j] > v) if (j === l) break;
    if (i >= j) break;
    swap(arr, i, j);
  }
  swap(arr, l, j);
  return j;
}

function quickSort(arr, l, r, len) {
  if (r - l + 1 <= len) return;
  const j = partition(arr, l, r);
  quickSort(arr, l, j - 1, len);
  quickSort(arr, j + 1, r, len);
}

function quickSortIgnoreLittleArr(arr, len) {
  shuffle(arr);
  quickSort(arr, 0, arr.length - 1, len);
  insertSort(arr);
  assert(isSorted(arr));
}

const times = 10; // 运行10次，求平均值

const data = [];

for (let m = 1;m <= 100;m++) {
  const num = 1e5;
  let sum = 0;
  for (let i = 0;i < times;i++) {
    const arr = randomDoubleArray(num);
    const start = Date.now();
    quickSortIgnoreLittleArr(arr,m);
    sum += (Date.now() - start);
  }
  data.push(sum / times);
}

const seriesData = [{
  name:'忽略小数组长度运行时间曲线',
  type:'line',
  data
}];

require('fs').writeFileSync('./27.series.json',JSON.stringify(seriesData));
