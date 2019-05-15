// 快速归并
// 实现一个merge方法，按照降序将a[]的后半部分复制到aux[],然后将其归并会a[]中，这样就可以去掉内循环中检测某半边是否用尽的代码，注意：这样排序的结果是不稳定的

class Merger {
  constructor(arr) {
    this.arr = arr;
    this.aux = new Array(arr.length);
  }

  merge(start, mid, end) {
    for (let k = start; k <= mid; k++) {
      this.aux[k] = this.arr[k];
    }
    const offset = mid + 1;
    for (let k = offset; k <= end; k++) {
      this.aux[k] = this.arr[end - k + offset];
    }
    let i = start, j = end;
    for (let k = start; k <= end; k++) {
      if (this.aux[i] < this.aux[j]) {
        this.arr[k] = this.aux[i++];
      } else {
        this.arr[k] = this.aux[j--];
      }
    }
  }
}

class MergeSortBottomUp extends Merger {
  constructor(arr) {
    super(arr);
  }

  sort() {
    const n = this.arr.length;
    for (let sz = 1; sz < n; sz *= 2) {
      for (let start = 0; start + sz < n; start += 2 * sz) {
        const mid = start + sz - 1;
        const end = Math.min(n - 1, mid + sz);
        this.merge(start, mid, end);
      }
    }
  }
}

class MergeSortTopDown extends Merger {
  constructor(arr) {
    super(arr);
  }

  sort() {
    this._mergeSort(0, this.arr.length - 1);
  }

  _mergeSort(start, end) {
    if (start >= end) return;
    const mid = start + parseInt((end - start) / 2);
    this._mergeSort(start, mid);
    this._mergeSort(mid + 1, end);
    this.merge(start, mid, end);
  }
}

const arr = [2, 1, 3, 4, 5, 7, -1];
const arr2 = arr.slice();
const m1 = new MergeSortBottomUp(arr);
const m2 = new MergeSortTopDown(arr2);
m1.sort();
m2.sort();
debugger;
