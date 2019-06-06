// 最佳情况

// 生成算法2.5中的sort方法表现最佳的数组（无重复元素）：数组大小为N并且不包含重复元素，每次切分后两个子数组的大小最多差1（子数组的大小和含有N个相同元素的数组的切分情况相同）。对于这道练习，我们不需要在排序开始的时候就打乱数组。

const assert = require('assert');
const _ = require('lodash');
const swap = require('../../swap');

// 中点两边是最佳情况，整个数组就是最佳情况了
// 首先构造一个有序数组，然后找到中点（作为枢轴），对中点左右两侧子数组进行构造，将选择的枢轴放到开始处(a[lo])。
function _quickBest(arr, lo, hi) {
  for (let k = lo; k <= hi; k++) {
    assert.deepStrictEqual(k, arr[k]);
  }
  if (hi <= lo) {
    return;
  }
  const mid = lo + parseInt((hi - lo) / 2);
  _quickBest(arr, lo, mid - 1);
  _quickBest(arr, mid + 1, hi);
  swap(arr, lo, mid);
}

function quickBest(n) {
  const arr = _.range(n);
  _quickBest(arr, 0, n - 1);
  return arr;
}

let ret = quickBest(3); // 1,0,2
ret = quickBest(5); // 2,1,0,3,4
ret = quickBest(6); // 2,1,0,4,3,5
debugger;
