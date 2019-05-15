// K 路归并

// 估计最佳的K值并通过实验验证猜想

const assert = require('assert');

const {isSorted} = require('../../util');

function _mergeSortKWays(arr, aux, start, end, k) {
  if (start >= end) return;
  let startNew = start;
  let endNew = 0;
  const step = parseInt((end - start) / k);
  for (let i = 0; i < k; i++) {
    endNew = startNew + step;
    if (endNew > end) {
      endNew = end;
    }
    _mergeSortKWays(arr, aux, startNew, endNew, k);
    startNew = endNew + 1;
    if (endNew === end) {
      break;
    }
  }
  _mergeKWays(arr, aux, start, end, k);
}

function _mergeKWays(arr, aux, start, end, k) {
  for (let i = start; i <= end; i++) {
    aux[i] = arr[i];
  }
  const bounds = []; // 记录每个区间的左右端点
  let startNew = start;
  let endNew = 0;
  const step = parseInt((end - start) / k);
  for (let i = 0; i < k; i++) {
    endNew = startNew + step;
    if (endNew > end) {
      endNew = end;
    }
    bounds.push([startNew, endNew]);
    startNew = endNew + 1;
    if (endNew === end) {
      break;
    }
  }
  let minIndex = 0;
  for (let i = start; i <= end; i++) {
    for (let j = 0; j < bounds.length; j++) {
      if (bounds[j][0] <= bounds[j][1] && bounds[j][0] <= end) {
        // 一个保底的minIndex
        minIndex = j;
        break;
      }
    }
    for (let j = 0; j < bounds.length; j++) {
      if (bounds[j][0] <= bounds[j][1] && bounds[j][0] <= end) {
        // 依次比较每个区间的开头的元素
        if (aux[bounds[j][0]] < aux[bounds[minIndex][0]]) {
          minIndex = j;
        }
      }
    }
    arr[i] = aux[bounds[minIndex][0]];
    bounds[minIndex][0]++; // ??
  }
}

function mergeSortKWays(arr, k) {
  assert(k > 1);
  const n = arr.length;
  const aux = new Array(n);
  _mergeSortKWays(arr, aux, 0, n - 1, k);
  assert(isSorted(arr));
}

const arr = [2, 1, 3, 5, 4,3,4,7,9,8];
mergeSortKWays(arr, 2);
debugger;
