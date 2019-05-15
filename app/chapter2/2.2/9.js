// 在库函数中使用aux[]这样的静态数组是不妥当的，因为可能会有多个程序同时使用这个类，实现一个不使用静态数组的Merge类，但也不要将aux[]变为merge()的局部变量
// 提示：可以使用辅助数组作为参数传递给递归的sort()方法

class Merger {
  constructor(arr) {
    this.arr = arr;
  }

  merge(start, mid, end, aux) {
    for (let k = start; k <= end; k++) {
      aux[k] = this.arr[k];
    }
    let i = start, j = mid + 1;
    for (let k = start; k <= end; k++) {
      if (i > mid) {
        this.arr[k] = aux[j++];
      } else if (j > end) {
        this.arr[k] = aux[i++];
      } else if (aux[i] < aux[j]) {
        this.arr[k] = aux[i++];
      } else {
        this.arr[k] = aux[j++];
      }
    }
  }
}

class MergeSortTopDown extends Merger {
  constructor(arr) {
    super(arr);
  }

  _mergeSort(start, end, aux) {
    if (start >= end) return;
    const mid = start + parseInt((end - start) / 2);
    this._mergeSort(start, mid, aux);
    this._mergeSort(mid + 1, end, aux);
    this.merge(start, mid, end, aux);
  }

  sort() {
    const aux = new Array(this.arr.length);
    this._mergeSort(0, this.arr.length - 1, aux);
  }
}

class MergeSortBottomUp extends Merger {
  constructor(arr) {
    super(arr);
  }

  sort() {
    const n = this.arr.length;
    const aux = new Array(this.arr.length);
    for (let sz = 1; sz < n; sz *= 2) {
      for (let start = 0; start + sz < n; start += 2 * sz) {
        const mid = start + sz - 1;
        const end = Math.min(n - 1, mid + sz);
        this.merge(start, mid, end, aux);
      }
    }
  }
}

const arr = [2, 1, 3, 4, 5, -1, 0];
const arr2 = arr.slice();
const m1 = new MergeSortBottomUp(arr);
const m2 = new MergeSortTopDown(arr2);
m1.sort();
m2.sort();
debugger;
