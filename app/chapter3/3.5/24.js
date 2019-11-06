class RangeChecker {
  constructor(ranges) {
    // 对区间的起始点进行排序
    this.ranges = ranges.sort((a, b) => a[0] - b[0]);
  }

  findRange(num) {
    // 二分查找
    let lo = 0, hi = this.ranges.length - 1;
    while (lo <= hi) {
      const mid = lo + parseInt((hi - lo) / 2);
      const range = this.ranges[mid];
      if (num >= range[0] && num <= range[1]) {
        return range;
      }
      if (num > range[1]) {
        lo = mid + 1;
      } else if (num < range[0]) {
        hi = mid - 1;
      } else {
        return null;
      }
    }
    return null;
  }
}

const rc = new RangeChecker([
  [1643, 2033],
  [5532, 7643],
  [8999, 10332],
  [5666653, 5669321]
]);

console.log(rc.findRange(9122));
console.log(rc.findRange(8122));
