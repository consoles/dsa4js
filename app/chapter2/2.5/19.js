const assert = require('assert');

// 线性对数时间计算两组排列之间的Kendall tau距离
// 参考：https://algs4.cs.princeton.edu/22mergesort/Inversions.java.html,https://algs4.cs.princeton.edu/25applications/KendallTau.java.html

function kendallTauDistance1(arr1, arr2) {
  assert.deepStrictEqual(arr1.length, arr2.length, 'array dimensions disagree');
  const n = arr1.length;
  const ainv = [];
  const bnew = [];
  for (let i = 0; i < n; i++) {
    ainv[arr1[i]] = i;
  }
  for (let i = 0; i < n; i++) {
    bnew[i] = ainv[arr2[i]];
  }
  return inventionsCount(bnew);
}

function inventionsCount(arr) {
  function _count(arr, aux, l, r) {
    let count = 0;
    if (l >= r) return count;
    const mid = l + parseInt((r - l) / 2);
    count += _count(arr, aux, l, mid);
    count += _count(arr, aux, mid + 1, r);
    count += _merge(arr, aux, l, mid, r);
    return count;
  }

  function _merge(arr, aux, l, mid, r) {
    let count = 0;
    for (let k = l; k <= r; k++) {
      aux[k] = arr[k];
    }
    let i = l, j = mid + 1;
    for (let k = l; k <= r; k++) {
      if (i > mid) {
        arr[k] = aux[j++];
      } else if (j > r) {
        arr[k] = aux[i++];
      } else if (arr[j] < arr[i]) {
        arr[k] = aux[j++];
        // [i,mid]是有序区,arr[i] > arr[j]，所以这个区间内的所有元素都大于arr[j]
        count += (mid - i + 1);
      } else {
        arr[k] = aux[i++];
      }
    }
    return count;
  }

  return _count(arr, new Array(arr.length), 0, arr.length - 1);
}

const arr1 = [0, 3, 1, 6, 2, 5, 4];
const arr2 = [1, 0, 3, 6, 4, 2, 5];

const distance = kendallTauDistance1(arr1, arr2);
debugger;
