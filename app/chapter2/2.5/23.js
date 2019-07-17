const assert = require('assert');

const swap = require('../../swap');

/**
 * 使用 Floyd-Rivest 算法进行优化
 * 我们期望第 k 大的元素位于 a[k] 附近，因此优先对 a[k] 附近的区域进行选择。每次切分时枢轴都选择 a[k]，先递归对样本区域选择，再对整个数组进行选择。
 */
function select(arr, k) {
  assert(k >= 0 && k < arr.length, 'select element out of bounds');

  function _partition(lo, hi) {
    const v = arr[lo];
    let i = lo, j = hi + 1;
    while (true) {
      while (arr[++i] < v) {
        if (i === hi) {
          break;
        }
      }
      while (arr[--j] > v) {
        if (j === lo) {
          break;
        }
      }
      if (i >= j) {
        break;
      }
      swap(arr, i, j);
    }
    swap(arr, j, lo);
    return j;
  }

  function _select(lo, hi) {
    while (hi > lo) {
      if (hi - lo > 600) {
        const n = hi - lo + 1;
        const i = k - lo + 1;
        const z = parseInt(Math.log(n));
        const s = parseInt(Math.exp(2 * z / 3) / 2);
        const sd = parseInt(Math.sqrt(z * s * (n - s) / n) * Math.sign(i - n / 2) / 2);
        const newLo = Math.max(lo, k - i * s / n + sd);
        const newHi = Math.min(hi, k + (n - i) * s / n + sd);
        _select(newLo, newHi);
      }
      swap(arr, lo, k);
      const j = _partition(lo, hi);
      if (j === k) {
        return arr[j];
      }
      if (j > k) {
        hi = j - 1;
      } else if (j < k) {
        lo = j + 1;
      }
    }
    return arr[lo];
  }

  return _select(0, arr.length - 1);
}

const arr = [2, 1, 4, 3, 5, 7, 6];
const ret = select(arr, 7);
debugger;
