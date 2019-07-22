class MergeSortData {
  constructor(n) {
    const data = [];
    while (n--) {
      data.push(Math.random());
    }
    this.data = data;
    // 左右区间端点
    this.l = -1;
    this.r = -1;
    this.mergeIndex = -1; // [0,mergeIndex]表示已经merge之后的区间
    // 当前正在处理的区间
    this.workingRangeColor = '#FF0000';
    // 没有处理的区间
    this.freeRangeColor = '#C0C0C0';
    // 已经正确merge的区间的颜色（即：有序区）
    this.mergeColor = '#00FF00';
  }

  * _merge(l, mid, r) {
    const aux = [];
    let i = l, j = mid + 1;
    for (let k = l; k <= r; k++) {
      if (i > mid) {
        aux.push(this.data[j++]);
      } else if (j > r) {
        aux.push(this.data[i++]);
      } else if (this.data[i] < this.data[j]) {
        aux.push(this.data[i++]);
      } else {
        aux.push(this.data[j++]);
      }
    }
    this.l = l;
    this.r = r;
    for (let k = l; k <= r; k++) {
      this.data[k] = aux[k - l];
      this.mergeIndex = k;
      yield this.renderData;
    }
  }

  * _sortTopDown(l, r) {
    if (l >= r) return;
    this.l = l;
    this.r = r;
    this.mergeIndex = -1;
    yield this.renderData;
    const mid = l + parseInt((r - l) / 2);
    yield* this._sortTopDown(l, mid);
    yield* this._sortTopDown(mid + 1, r);
    yield* this._merge(l, mid, r);
  }

  * sortTopDown() {
    yield this.renderData;
    yield* this._sortTopDown(0, this.data.length - 1);
    this.l = 0;
    this.r = this.data.length - 1;
    this.mergeIndex = this.data.length - 1;
    yield this.renderData;
  }

  * sortDownTop() {
    yield this.renderData;
    for (let sz = 1; sz < this.data.length; sz *= 2) {
      for (let i = 0; i < this.data.length - sz; i += 2 * sz) {
        // 对arr[i...i+sz - 1]和arr[i+sz...i+sz*2-1]的区间进行合并
        const l = i;
        const mid = i + sz - 1;
        const r = Math.min(this.data.length - 1, i + 2 * sz - 1);
        yield* this._merge(l, mid, r);
      }
    }
  }

  get renderData() {
    const data = [];
    for (let i = 0; i < this.data.length; i++) {
      let color = this.freeRangeColor;
      if (i >= this.l && i <= this.r) {
        color = this.workingRangeColor;
      }
      if (i >= this.l && i <= this.mergeIndex) {
        color = this.mergeColor;
      }
      data.push({index: i, value: this.data[i], color});
    }
    return data;
  }
}
