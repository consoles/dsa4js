const swap = require('../../swap');

class MaxPQ {
  /**
   * @param arr
   * @param bottomUp 是否自底向上建堆
   */
  constructor(arr, bottomUp = false) {
    this.arr = [-1].concat(arr);
    if (bottomUp)
      this.initBottomUp();
    else
      this.initTopDown();
  }

  _swim(k) {
    while (k > 1) {
      const parentIndex = parseInt(k / 2);
      if (this.arr[k] > this.arr[parentIndex]) {
        swap(this.arr, k, parentIndex);
        k = parentIndex;
      } else {
        break;
      }
    }
  }

  _sink(k) {
    const maxIndex = this.arr.length - 1;
    while (k < maxIndex) {
      let j = 2 * k;
      if (j > maxIndex) break;
      if (j + 1 <= maxIndex && this.arr[j] < this.arr[j + 1]) {
        j++;
      }
      if (this.arr[j] <= this.arr[k]) {
        break;
      }
      swap(this.arr, j, k);
      k = j;
    }
  }

  initBottomUp() {
    // 叶子节点（没有子节点）自然而然是堆
    // 从第一个非叶子节点逐层向上，直到根节点执行sink
    for (let i = parseInt(this.arr.length / 2); i >= 1; i--) {
      this._sink(i);
    }
  }

  initTopDown() {
    // 当前元素不断swim直到根节点（就像向优先队列中插入元素一样）
    for (let i = 2; i < this.arr.length; i++) {
      this._swim(i);
    }
  }
}

const q = new MaxPQ([1, 2, 3, 4, 5, 6, 7, 8, 9],true);
debugger;
