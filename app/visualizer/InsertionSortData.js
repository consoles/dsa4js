class InsertionSortData {
  constructor(n, dataType = 'default') {
    const data = [];
    while (n--) {
      data.push(Math.random());
    }
    this.data = data;
    if (dataType === 'nearlyOrdered') {
      this.data.sort((a, b) => a - b);
      let swapTimes = 2;
      while (swapTimes--) {
        const i = Math.floor(Math.random() * this.dataLength);
        const j = Math.floor(Math.random() * this.dataLength);
        this.swap(i, j);
      }
    }
    this.orderedIndex = -1;
    this.currentIndex = -1;
    this.orderColor = '#00FF00';
    this.unOrderColor = '#C0C0C0';
    this.currentColor = '#FF0000';
  }

  get dataLength() {
    return this.data.length;
  }

  less(i, j) {
    return this.data[i] < this.data[j];
  }

  swap(i, j) {
    if (i === j) return;
    [this.data[i], this.data[j]] = [this.data[j], this.data[i]];
  }

  * sort() {
    yield this.renderData;
    for (let i = 1; i < this.dataLength; i++) {
      this.currentIndex = i;
      yield this.renderData;
      for (let j = i; j > 0 && this.less(j, j - 1); j--) {
        this.swap(j, j - 1);
        this.currentIndex = j - 1;
        this.orderedIndex = i;
        yield this.renderData;
      }
      this.orderedIndex = i;
      this.currentIndex = -1;
      yield this.renderData;
    }
  }

  get renderData() {
    const data = [];
    for (let i = 0; i < this.dataLength; i++) {
      let color = this.unOrderColor;
      if (i === this.currentIndex) {
        color = this.currentColor;
      } else if (i <= this.orderedIndex) {
        color = this.orderColor;
      } else {
        color = this.unOrderColor;
      }
      data.push({index: i, value: this.data[i], color});
    }
    return data;
  }
}
