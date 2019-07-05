class MaxPQ {
  constructor() {
    this.data = [-1];
    this.sz = 0;
    this.minValue = null;
  }

  isEmpty() {
    return this.sz === 0;
  }

  _swim(k) {
    const key = this.data[k];
    while (k > 1 && this.data[parseInt(k / 2)] < key) {
      this.data[k] = this.data[parseInt(k / 2)];
      k = parseInt(k / 2);
    }
    this.data[k] = key;
  }

  insert(value) {
    const sz = ++this.sz;
    this.data[sz] = value;
    this.minValue = this.minValue === null?value:Math.min(value,this.minValue);
    this._swim(sz);
  }

  _sink(k) {
    const sz = this.sz;
    const key = this.data[k];
    while (2 * k <= sz) {
      let j = 2 * k;
      if (j + 1 <= sz && this.data[j + 1] > this.data[j]) {
        j++;
      }
      if (this.data[j] <= key) {
        break;
      }
      this.data[k] = this.data[j];
      k = j;
    }
    this.data[k] = key;
  }

  delMax() {
    const value = this.data[1];
    const sz = this.sz--;
    [this.data[1], this.data[sz]] = [this.data[sz], this.data[1]];
    this._sink(1);
    if (this.isEmpty()) {
      this.minValue = null;
    }
    return value;
  }

  min() {
    return this.minValue;
  }
}

// 保存最小值，在插入元素的时候进行比较更新
// 堆为空的时候更新最小值为空（因为每次都是删除的最大值，最小值还是在堆中）
