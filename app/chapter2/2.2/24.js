// 改进的有序测试

// 在实验中用大型随机数组评估练习2.2.8所做的修改的效果，根据经验用N（被排序的原始数组的大小）的函数描述条件语句（arr[mid] <= arr[mid + 1]）成立（无论数组是否有序）的平均次数

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
    this.hit = 0;
  }

  _mergeSort(start, end, aux) {
    if (start >= end) return;
    const mid = start + parseInt((end - start) / 2);
    this._mergeSort(start, mid, aux);
    this._mergeSort(mid + 1, end, aux);

    if (this.arr[mid] <= this.arr[mid + 1]) {
      this.hit++;
      return;
    }

    this.merge(start, mid, end, aux);
  }

  sort() {
    const aux = new Array(this.arr.length);
    this._mergeSort(0, this.arr.length - 1, aux);
  }
}

const T = 10;
let n = 1000;
while (true) {
  let sum = 0;
  for (let i = 0;i < T;i++) {
    const arr = [];
    for (let j = 0; j < n; j++) {
      arr.push(Math.random());
    }
    const m = new MergeSortTopDown(arr);
    m.sort();
    sum += m.hit;
  }
  console.log('n = ', n, 'hit = ', sum / T);
  n *= 10;
}

// n =  1000 hit =  295
// n =  10000 hit =  2866.6
// n =  100000 hit =  28622.8
// n =  1000000 hit =  292256.8
