class QuickSortData {
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
        const i = Math.floor(Math.random() * this.data.length);
        const j = Math.floor(Math.random() * this.data.length);
        this._swap(i, j);
      }
    } else if (dataType === 'identical') {
      // 数据内容全部一样
      this.data.fill(0.77);
    }
    this.l = -1;
    this.r = -1;
    this.curPivot = -1;
    this.curElement = -1;
    this.fixedPivots = new Array(this.data.length).fill(false);
    this.workingRangeColor = '#f00';
    this.idleRangeColor = '#ccc';
    this.curPivotColor = '#f0f';
    this.curElementColor = '#00f';
    this.fixedPivotsColor = '#0f0';
  }

  get renderData() {
    const data = [];
    for (let i = 0; i < this.data.length; i++) {
      let color = this.idleRangeColor;
      if (i >= this.l && i <= this.r) {
        // 正在处理的区间
        color = this.workingRangeColor;
      }
      if (i === this.curPivot) {
        color = this.curPivotColor;
      }
      if (i === this.curElement) {
        color = this.curElementColor;
      }
      if (this.fixedPivots[i]) {
        color = this.fixedPivotsColor;
      }
      data.push({index: i, value: this.data[i], color});
    }
    return data;
  }

  _swap(i, j) {
    [this.data[i], this.data[j]] = [this.data[j], this.data[i]];
  }

  * _partition(l, r) {

    this.l = l;
    this.r = r;
    this.curElement = -1;

    // 随机选择标定点来优化对于近乎有序的数组快排退化为O(N^2)
    const index = Math.floor(Math.random() * (r - l + 1)) + l;

    this.curPivot = index;
    yield this.renderData;

    this._swap(index, l);

    this.curPivot = l;
    yield this.renderData;

    const v = this.data[l];

    let j = l;
    for (let i = l + 1; i <= r; i++) {
      this.curElement = i;
      yield this.renderData;
      if (this.data[i] < v) {
        this._swap(++j, i);
        yield this.renderData; // data发生改变后重新绘制
      }
    }
    this._swap(l, j);
    this.fixedPivots[j] = true;
    this.curElement = -1;
    yield this.renderData;
    return j;
  }

  * _quickSort(l, r) {
    if (l >= r) {
      if (l === r) {
        this.l = l;
        this.r = r;
        this.fixedPivots[l] = true;
        this.curElement = -1;
        this.curPivot = -1;
        yield this.renderData;
      }
      return;
    }
    this.l = l;
    this.r = r;
    this.curElement = -1;
    this.curPivot = -1;
    yield this.renderData;
    // 调用另一个有返回值的生成器函数
    const j = yield* this._partition(l, r);
    yield* this._quickSort(l, j - 1);
    yield* this._quickSort(j + 1, r);
  }

  * sort() {
    yield this.renderData;
    yield* this._quickSort(0, this.data.length - 1);
  }
}

class QuickSort2WaysData extends QuickSortData {
  constructor(n, dataType) {
    super(n, dataType);
    // 每次扫描过程中左右两个索引应该停止的位置
    this.curL = -1;
    this.curR = -1;
    this.rangeColor = '#FFAE8F';
  }

  * _partition(l, r) {
    const index = Math.floor(Math.random() * (r - l + 1)) + l;
    this.curPivot = index;
    yield this.renderData;
    this._swap(index, l);
    this.curPivot = l;
    yield this.renderData;
    const v = this.data[l];
    let i = l + 1, j = r;
    this.l = l;
    this.r = r;
    this.curL = i;
    this.curR = j;
    while (true) {
      while (i <= r && this.data[i] < v) {
        i++;
        this.curL = i;
        yield this.renderData;
      }
      while (j >= l + 1 && this.data[j] > v) {
        j--;
        this.curR = j;
        yield this.renderData;
      }

      if (i >= j) break;
      this._swap(i, j);
      i++;
      j--;
      this.curL = i;
      this.curR = j;
      yield this.renderData;
    }
    this._swap(l, j);
    this.fixedPivots[j] = true;
    yield this.renderData;
    return j;
  }

  get renderData() {
    const data = [];
    for (let i = 0; i < this.data.length; i++) {
      let color = this.idleRangeColor;
      if (i >= this.l && i <= this.r) {
        // 正在处理的区间
        color = this.workingRangeColor;
      }
      if (i >= this.l + 1 && i <= this.curL) {
        color = this.rangeColor;
      }
      if (i >= this.curR && i <= this.r) {
        color = this.rangeColor;
      }
      if (i === this.curPivot) {
        color = this.curPivotColor;
      }
      if (i === this.curElement) {
        color = this.curElementColor;
      }
      if (this.fixedPivots[i]) {
        color = this.fixedPivotsColor;
      }
      data.push({index: i, value: this.data[i], color});
    }
    return data;
  }

  * _quickSort(l, r) {
    if (l >= r) {
      if (l === r) {
        this.l = l;
        this.r = r;
        this.fixedPivots[l] = true;
        this.curL = -1;
        this.curR = -1;
        this.curElement = -1;
        this.curPivot = -1;
        yield this.renderData;
      }
      return;
    }
    this.l = l;
    this.r = r;
    this.curElement = -1;
    this.curPivot = -1;
    yield this.renderData;
    // 调用另一个有返回值的生成器函数
    const j = yield* this._partition(l, r);
    yield* this._quickSort(l, j - 1);
    yield* this._quickSort(j + 1, r);
  }

  * sort() {
    yield this.renderData;
    yield* this._quickSort(0, this.data.length - 1);
    this.l = -1;
    this.r = -1;
    this.curPivot = -1;
    this.curL = -1;
    this.curR = -1;
    yield this.renderData;
  }
}

class QuickSort3WaysData extends QuickSort2WaysData {
  constructor(n, dataType) {
    super(n, dataType);
    this.ltColor = '#7C85DD';
    this.eqColor = '#CC6D03';
    this.gtColor = '#15CC48';
    this.lt = -1;
    this.gt = -1;
  }

  * _quickSort(l, r) {
    if (l >= r) {
      if (l === r) {
        this.l = l;
        this.r = r;
        this.fixedPivots[l] = true;
        this.lt = -1;
        this.gt = -1;
        this.curElement = -1;
        this.curPivot = -1;
        yield this.renderData;
      }
      return;
    }

    const index = Math.floor(Math.random() * (r - l + 1)) + l;
    this.curPivot = index;
    yield this.renderData;
    this._swap(index, l);
    this.curPivot = l;
    yield this.renderData;

    const v = this.data[l];

    let lt = l;
    let gt = r;
    let i = l + 1;

    this.lt = lt;
    this.gt = gt;
    yield this.renderData;

    // < v: [l...lt-1]
    // = v: [lt...gt]
    // > v:[gt+1...j]
    while (i <= gt) {
      if (this.data[i] < v) {
        this._swap(i++, lt++);
        this.lt = lt;
        yield this.renderData;
      } else if (this.data[i] > v) {
        this._swap(i, gt--);
        this.gt = gt;
        yield this.renderData;
      } else {
        i++;
      }
    }

    for (let k = lt; k <= gt; k++) {
      this.fixedPivots[k] = true;
    }
    yield this.renderData;

    yield* this._quickSort(l, lt - 1);
    yield* this._quickSort(gt + 1, r);
  }

  get renderData() {
    const data = [];
    for (let i = 0; i < this.data.length; i++) {
      let color = this.idleRangeColor;
      if (i >= this.l && i <= this.r) {
        // 正在处理的区间
        color = this.workingRangeColor;
      }
      if (i >= this.l && i < this.lt) {
        color = this.ltColor;
      }
      if (i >= this.lt && i <= this.gt) {
        color = this.eqColor;
      }
      if (i > this.gt && i <= this.r) {
        color = this.gtColor;
      }
      if (i === this.curPivot) {
        color = this.curPivotColor;
      }
      if (i === this.curElement) {
        color = this.curElementColor;
      }
      if (this.fixedPivots[i]) {
        color = this.fixedPivotsColor;
      }
      data.push({index: i, value: this.data[i], color});
    }
    return data;
  }
}
