const swap = require('../../swap');

class MaxHeap {
  constructor(arr) {
    this.arr = [];
    this.size = 0;
    for (const item of arr) {
      this.insert(item);
    }
  }

  _swim(k) {
    while (k > 1) {
      const parentIndex = Math.floor(k / 2);
      if (this.arr[k] <= this.arr[parentIndex]) break;
      swap(this.arr, k, parentIndex);
      k = parentIndex;
    }
  }

  insert(item) {
    this.arr[++this.size] = item;
    this._swim(this.size);
  }

  _sink(k) {
    const sz = this.size;
    // 判断孩子节点中是否有比这个大的
    while (2 * k <= sz) {
      let j = 2 * k;
      if (j < sz && this.arr[j] < this.arr[j + 1]) j++;
      if (this.arr[k] >= this.arr[j]) break;
      swap(this.arr, k, j);
      k = j;
    }
  }

  deleteMax() {
    const item = this.arr[1];

    swap(this.arr, 1, this.size--);
    this.arr[this.size + 1] = null;
    this._sink(1);

    return item;
  }

  isEmpty() {
    return this.size === 0;
  }
}

class MaxHeap2 extends MaxHeap{

  _sink(k) {
    const sz = this.size;
    // 判断孩子节点中是否有比这个大的
    while (2 * k <= sz) {
      let j = 2 * k;
      if (this.arr[j] < this.arr[j + 1]) j++;
      if (this.arr[k] >= this.arr[j]) break;
      swap(this.arr, k, j);
      k = j;
    }
  }

  deleteMax() {
    const item = this.arr[1];
    swap(this.arr,1,this.size--);
    this.arr[this.size + 1] = this.arr[1]; // 最后一个元素的值等于将要下沉的值，这样下沉的时候元素会由于相等而跳过
    this._sink(1);
    this.arr[this.size + 1] = null;
    return item;
  }
}

// const arr = [2,1,3,5,4,7,9,8,6,0];
// const heap = new MaxHeap(arr);
// debugger;
// while (!heap.isEmpty()) {
//   console.log(heap.deleteMax());
// }
// debugger;
