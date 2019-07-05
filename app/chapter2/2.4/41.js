// 任意一个节点i的子节点范围[d(i-1) + 2,di + 1]
// 父节点Math.ceil((i - 2) / d)

const assert = require('assert');

const {isSorted, genRandomDoubleArray} = require('../../util');

class HeapSortMultiWay {
  constructor(d) {
    this.compareCount = 0;
    this.d = d;
  }

  _sink(arr, k, n) {
    const d = this.d;
    while ((k - 1) * d + 2 <= n) {
      let j = d * (k - 1) + 2;
      // 在d个节点中找到最大的那个
      for (let i = 0, q = j; i < d; i++) {
        if (q + i <= n && this._less(arr, j, q + i)) {
          j = q + i;
        }
      }
      if (this._less(arr, k, j)) {
        this._swap(arr, k, j);
        k = j;
      } else {
        break;
      }
    }
  }

  _less(arr, i, j) {
    this.compareCount++;
    return arr[i - 1] < arr[j - 1];
  }

  _swap(arr, i, j) {
    [arr[i - 1], arr[j - 1]] = [arr[j - 1], arr[i - 1]];
  }

  sort(arr) {
    let n = arr.length;
    const d = this.d;
    for (let k = Math.ceil((n - 2) / d); k >= 1; k--) {
      this._sink(arr, k, n);
    }
    while (n > 1) {
      this._swap(arr, 1, n--);
      this._sink(arr, 1, n);
    }
  }
}

class HeapSort {

  constructor() {
    this.compareCount = 0;
  }

  sort(arr) {
    let n = arr.length;
    // 建堆
    for (let k = parseInt(n / 2); k >= 1; k--) {
      this._sink(arr, k, n);
    }
    while (n > 1) {
      this._swap(arr, 1, n--);
      this._sink(arr, 1, n);
    }
  }

  _sink(arr, k, n) {
    while (2 * k <= n) {
      let j = 2 * k;
      if (j < n && this._less(arr, j, j + 1)) {
        j++;
      }
      if (this._less(arr, k, j)) {
        this._swap(arr, j, k);
        k = j;
      } else {
        break;
      }
    }
  }

  _less(arr, i, j) {
    this.compareCount++;
    return arr[i - 1] < arr[j - 1];
  }

  _swap(arr, i, j) {
    [arr[i - 1], arr[j - 1]] = [arr[j - 1], arr[i - 1]];
  }
}

const testCount = 10;
for (let n = 1e4;n <= 1e7;n *=10) {
  const totalTime = {
    std:0,
    s3:0,
    s4:0
  };
  const totalCompareCount = {
    std:0,
    s3:0,
    s4:0
  };
  for (let i = 0; i < testCount; i++) {
    const arr1 = genRandomDoubleArray(n);
    const arr2 = arr1.slice();
    const arr3 = arr1.slice();
    const sStd = new HeapSort();
    const s3 = new HeapSortMultiWay(3);
    const s4 = new HeapSortMultiWay(4);
    const start = Date.now();
    sStd.sort(arr1);
    const endStd = Date.now();
    s3.sort(arr2);
    const endS3 = Date.now();
    s4.sort(arr3);
    const endS4 = Date.now();
    totalTime.std += (endStd - start);
    totalTime.s3 += (endS3 - endStd);
    totalTime.s4 += (endS4 - endS3);
    totalCompareCount.std += sStd.compareCount;
    totalCompareCount.s3 += s3.compareCount;
    totalCompareCount.s4 += s4.compareCount;
    assert(isSorted(arr1));
    assert(isSorted(arr2));
    assert(isSorted(arr3));
  }
  let text = `n = ${n}\t耗时`;
  for (const key of Object.keys(totalTime)) {
    text += `${key}:${totalTime[key] / testCount}\t`;
  }
  text+='\t比较次数:';
  for (const key of Object.keys(totalCompareCount)) {
    text += `${key}:${totalCompareCount[key] / testCount}\t`;
  }
  console.log(text);
}

// n = 10000       耗时std:13.4    s3:11.9 s4:7.8          比较次数:std:235397.3   s3:302198.6     s4:303564.8
// n = 100000      耗时std:117.7   s3:116  s4:92           比较次数:std:3019734.1  s3:3863181.4    s4:3846997.6
// n = 1000000     耗时std:1617.2  s3:1382.1       s4:1194.4               比较次数:std:36793783.8 s3:47056705.2   s4:46939798.9
// n = 10000000    耗时std:21974.6 s3:17540.4      s4:15261                比较次数:std:434643707.7        s3:554782389    s4:552569848.3
