// 螺丝和螺帽

// 假设有n个螺丝和n个螺帽混在一对，你需要快速将它们配对。一个螺丝只会匹配一个螺帽，一个螺帽也只会匹配一个螺丝(螺丝和螺帽都是不重复的)。你可以尝试着把一个螺丝和一个螺帽拧在一起看看谁大了，但是不能直接比较2个螺丝或者2个螺帽。给出一个解决问题的有效方法

const assert = require('assert');
const _ = require('lodash');
const swap = require('../../swap');

function partition(bolts, nuts, lo, hi) {
  let i = lo, j = hi + 1;
  const vB = bolts[lo]; // 螺母的标定点
  // 一:找到和vB螺母配对的螺丝,并交换，此时nuts[lo]和vB就对应了
  for (let k = lo; k <= hi; k++) {
    if (nuts[k] === vB) {
      swap(nuts, k, lo);
      break;
    }
  }
  // 二:使用螺母作为标定点，对螺丝进行排序
  while (true) {
    while (nuts[++i] < vB) {
      if (i === hi) {
        break;
      }
    }
    while (nuts[--j] > vB) {
      if (j === lo) {
        break;
      }
    }
    if (i >= j) {
      break;
    }
    swap(nuts, i, j);
  }
  swap(nuts, lo, j);
  // 用螺丝去比较螺母（经过上述的操作nuts[j]位置的螺母放到了正确的位置）
  // assert(vN === vB); // 这里的vN其实是等于vB的，因为开始的vB = bots[lo],步骤一过后:vB = bots[lo] = nuts[lo],步骤2后nuts[lo]被放到了j位置
  const vN = nuts[j];
  i = lo;
  j = hi + 1;
  while (true) {
    while (bolts[++i] < vN) {
      if (i === hi) {
        break;
      }
    }
    while (bolts[--j] > vN) {
      if (j === lo) {
        break;
      }
    }
    if (i >= j) {
      break;
    }
    swap(bolts, i, j);
  }
  swap(bolts, lo, j);
  return j;
}

function _sortBoltsAndButs(bolts, nuts, lo, hi) {
  if (hi <= lo) return;
  const j = partition(bolts, nuts, lo, hi);
  _sortBoltsAndButs(bolts, nuts, lo, j - 1);
  _sortBoltsAndButs(bolts, nuts, j + 1, hi);
}

function sortBoltsAndButs(bolts, nuts) {
  assert.deepEqual(bolts.length, nuts.length, '螺丝和螺母不配套');
  _sortBoltsAndButs(bolts, nuts, 0, bolts.length - 1);
}

const indexes = _.range(0, 9);

const bolts = _.shuffle(indexes); // 螺母数组
const nuts = _.shuffle(indexes); // 螺母数组

sortBoltsAndButs(bolts,nuts);
debugger;
