// 切换为插入排序

// 实现一个快排，在子数组元素少于M时切换到插入排序。用快排处理大小N分别为10^3,10^4,10^5,10^6的随机数组，根据经验给出使其在你的计算机上运行速度最快的M值，将M从0变化到30的每个值所得到的平均运行时间绘制成曲线。注意：你需要为算法2.2天假一个需要3个参数的sort()方法以使`Insertion.sort(arr,lo,hi)`将子数组arr[lo...hi]排序

const assert = require('assert');

const _ = require('lodash');

const swap = require('../../swap');
const {shuffle, isSorted,randomDoubleArray} = require('../../util');
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

const mArr = _.range(5,11);
const numArr = [1e3,1e4,1e5,1e6];
const times = 10; // 运行10次，求平均值

const counter = {};

for (const m of mArr) {
  counter[m] = [];
  for (const num of numArr) {
    let sum = 0;
    for (let i = 0;i < times;i++) {
      const arr = randomDoubleArray(num);
      const start = Date.now();
      quickSort(arr,m);
      sum += (Date.now() - start);
    }
    counter[m].push(sum / times);
  }
}

const seriesData = Object.keys(counter).map(m => ({
  name:String(m),
  type:'line',
  data:counter[m]
}));

require('fs').writeFileSync('./25.series.json',JSON.stringify(seriesData));
