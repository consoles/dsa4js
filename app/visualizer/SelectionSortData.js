class SelectionSortData {
  constructor(n) {
    const data = [];
    while (n--) {
      data.push(Math.random());
    }
    this.orderedIndex = -1; // [0,orderedIndex]是有序的
    this.currentMinIndex = -1; // 当前找到最小元素的索引
    this.currentCompareIndex = -1; // 当前正在比较的元素索引
    this.data = data;
    this.orderColor = '#00FF00';
    this.unOrderColor = '#C0C0C0';
    this.minIndexColor = '#0000FF';
    this.compareIndexColor = '#FF0000';
  }

  get dataLength() {
    return this.data.length;
  }

  less(i, j) {
    return this.data[i] < this.data[j];
  }

  swap(i, j) {
    [this.data[i], this.data[j]] = [this.data[j], this.data[i]];
  }

  * sort() {
    yield this.renderData;
    for (let i = 0; i < this.dataLength; i++) {
      let minIndex = i;
      this.currentMinIndex = minIndex;
      yield this.renderData;
      for (let j = i + 1; j < this.dataLength; j++) {
        this.currentCompareIndex = j;
        yield this.renderData;
        if (this.less(j, minIndex)) {
          minIndex = j;
          this.currentMinIndex = minIndex;
          yield this.renderData;
        }
      }

      if (minIndex !== i) {
        this.swap(minIndex, i);
      }
      this.currentMinIndex = -1;
      this.currentCompareIndex = -1;
      this.orderedIndex = i;
      yield this.renderData;
    }
  }

  get renderData() {
    const data = [];
    for (let i = 0; i < this.dataLength; i++) {
      let color = this.unOrderColor;
      if (i <= this.orderedIndex) {
        color = this.orderColor;
      } else if (i === this.currentCompareIndex) {
        color = this.compareIndexColor;
      } else if (i === this.currentMinIndex) {
        color = this.minIndexColor;
      }
      data.push({index: i, value: this.data[i], color});
    }
    return data;
  }
}
