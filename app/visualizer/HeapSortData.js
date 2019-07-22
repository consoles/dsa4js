class HeapSortData {
  constructor(n) {
    const data = [];
    while (n--) {
      data.push(Math.random());
    }
    this.data = data;
    this.heapIndex = 0; // 堆有序[heapIndex,n)
    this.orderedColor = '#0f0';
    this.unOrderedColor = '#ccc';
  }

  get renderData() {
    const data = [];
    for (let i = 0; i < this.data.length; i++) {
      const color = i >= this.heapIndex ? this.orderedColor : this.unOrderedColor;
      data.push({index: i, value: this.data[i], color});
    }
    return data;
  }

  _sink(k, n) {
    while (2 * k + 1 < n) {
      let j = 2 * k + 1;
      if (j + 1 < n && this.data[j + 1] > this.data[j]) {
        j++;
      }
      if (this.data[k] < this.data[j]) {
        this._swap(k, j);
        k = j;
      } else {
        break;
      }
    }
  }

  _swap(i, j) {
    [this.data[i], this.data[j]] = [this.data[j], this.data[i]];
  }

  * sort() {
    let n = this.data.length;
    this.heapIndex = n;
    yield this.renderData;

    // 建堆
    for (let k = parseInt(n / 2); k >= 0; k--) {
      this._sink(k, n);
    }

    while (n > 0) {
      this._swap(0, --n);
      this._sink(0, n);
      this.heapIndex = n;
      yield this.renderData;
    }
    this.heapIndex = 0;
    yield this.renderData;
  }
}
