// 编写一段代码来计算Cn的准确值，在N=100,1000和10000的情况下比较准确值和估计2NlnN的差距

const {shuffle, genRandomDoubleArray} = require('../../util');
const swap = require('../../swap');

class QuickSortAnalyze {
  constructor(arr) {
    this.arr = shuffle(arr);
    this.compareCount = 0;
  }

  less(v1, v2) {
    this.compareCount++;
    return v1 < v2;
  }

  _partition(lo, hi) {
    const v = this.arr[lo];
    let i = lo, j = hi + 1;
    while (true) {
      while (this.less(this.arr[++i], v)) {
        if (i === hi) {
          break;
        }
      }
      while ((this.less(v, this.arr[--j]))) {
        if (j === lo) {
          break;
        }
      }
      if (i >= j) {
        break;
      }
      swap(this.arr, i, j);
    }
    swap(this.arr, lo, j);
    return j;
  }

  _quickSort(lo, hi) {
    if (lo >= hi) return;
    const j = this._partition(lo, hi);
    this._quickSort(lo, j - 1);
    this._quickSort(j + 1, hi);
  }

  sort() {
    this._quickSort(0, this.arr.length - 1);
  }
}

const nums = [100, 1000, 10000];
const TEST_COUNT = 10;

for (const num of nums) {
  let sum = 0;
  for (let i = 0; i < TEST_COUNT; i++) {
    const arr = genRandomDoubleArray(num);
    const s = new QuickSortAnalyze(arr);
    s.sort();
    sum += s.compareCount;
  }
  const averageCompare = sum / TEST_COUNT;
  const estimatedCompare = 2 * num * Math.log2(num);
  console.log('数据规模', num, '理论值(2N*lgN)', estimatedCompare, '实际值', averageCompare, '比例(实际/理论)', averageCompare / estimatedCompare);
}

// 数据规模 100 理论值(2N*lgN) 1328.7712379549448 实际值 754.4 比例(实际/理论) 0.5677425718222686
// 数据规模 1000 理论值(2N*lgN) 19931.568569324176 实际值 12181.2 比例(实际/理论) 0.6111510971970147
// 数据规模 10000 理论值(2N*lgN) 265754.247590989 实际值 168545.1 比例(实际/理论) 0.634214134027316
