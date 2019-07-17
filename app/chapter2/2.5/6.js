// 实现select

const assert = require('assert');

const swap = require('../../swap');
const {shuffle} = require('../../util');

class QuickPedantic {
  sort(arr) {
    shuffle(arr);
    this._sort(arr, 0, arr.length - 1);
  }

  _sort(arr, lo, hi) {
    if (lo >= hi) return;
    const j = this._partition(arr, lo, hi);
    this._sort(arr, lo, j - 1);
    this._sort(arr, j + 1, hi);
  }

  _partition(arr, lo, hi) {
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

  select(arr, k) {
    assert(k >= 0 && k < arr.length, 'selected elements out of bounds');
    shuffle(arr);
    let lo = 0, hi = arr.length - 1;
    while (hi > lo) {
      const i = this._partition(arr, lo, hi);
      if (i > k) {
        hi = i - 1;
      } else if (i < k) {
        lo = i + 1;
      } else {
        return arr[i];
      }
    }
    return arr[lo];
  }

  select2(arr, k) {
    assert(k >= 0 && k < arr.length, 'selected elements out of bounds');
    return this._select2(arr, k, 0, arr.length - 1);
  }

  /**
   * 递归版本
   */
  _select2(arr, k, lo, hi) {
    if (lo >= hi) return arr[lo];
    const j = this._partition(arr, lo, hi);
    if (j === k) return arr[j];
    return j > k ? this._select2(arr, k, lo, j - 1) : this._select2(arr, k, j + 1, hi);
  }
}
