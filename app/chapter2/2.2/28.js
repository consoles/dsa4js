// 自顶向下和自底向上

// 对于N = 10^3~10^6使用SortCompare比较自顶向下和自底向上归并排序的性能

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

const genRandomArr = N => {
  const arr = [];
  for (let i = 0; i < N; i++) {
    arr.push(Math.random());
  }
  return arr;
};

const time = (Sort, T, N) => {
  let total = 0;
  for (let i = 0; i < T; i++) {
    const arr = genRandomArr(N);
    const sorter = new Sort(arr);
    const start = Date.now();
    sorter.sort(arr);
    total += (Date.now() - start);
  }
  return total / T;
};

(() => {
  const T = 10;

  for (let n = 1e3; n <= 1e6; n *= 10) {

    const t1 = time(MergeSortTopDown, T, n);
    const t2 = time(MergeSortBottomUp, T, n);

    console.log(`T = ${T},N = ${n}`);
    console.log('自顶向下', t1);
    console.log('自底向上', t2);
    console.log('自顶向下 / 自底向上', t1 / t2);
    console.log();
  }
})();

// T = 10,N = 1000
// 自顶向下 0.7
// 自底向上 0.7
// 自顶向下 / 自底向上 1
//
// T = 10,N = 10000
// 自顶向下 1.7
// 自底向上 1.2
// 自顶向下 / 自底向上 1.4166666666666667
//
// T = 10,N = 100000
// 自顶向下 19
// 自底向上 16.6
// 自顶向下 / 自底向上 1.1445783132530118
//
// T = 10,N = 1000000
// 自顶向下 219.5
// 自底向上 185.6
// 自顶向下 / 自底向上 1.1826508620689655
