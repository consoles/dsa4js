const assert = require('assert');

class MultiDimensionData {
  constructor(data) {
    assert(Array.isArray(data) && data.length > 0);
    this.data = data; // 第0维的数据存储在data[0]，第一维的数据存储在data[1]
    this.desc = data.join(',');
  }

  get dimension() {
    return this.data.length;
  }

  /**
   * 类似于字符串比较的逐个比较
   * @param other
   * @returns {number}
   */
  compareTo(other) {
    for (let d = 0; d < this.dimension; d++) {
      const diff = this.data[d] - other.data[d];
      if (diff !== 0) return diff;
    }
    return 0;
  }
}

const datas = [[1, 1, 2], [1, 2, 1], [1, 1, 3], [2, 1, 1], [1, 3, 1]].map(x => new MultiDimensionData(x));
datas.sort((a, b) => a.compareTo(b));
debugger;
