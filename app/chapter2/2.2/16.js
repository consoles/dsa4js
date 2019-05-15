// 自然的归并排序

// 编写一个自底向上的归并排序，当需要将2个子数组排序时能够利用数组中已经有序的部分
// 首先找到一个有序的子数组（移动指针直到当前元素比上一个元素小为止），然后再找出另一个将它们归并
// 根据数组大小和数组中递增子数组的最大长度分析算法的运行时间

const assert = require('assert');

const {isSorted} = require('../../util');

function merge(arr, aux, l, mid, r) {
  for (let i = l; i <= r; i++) {
    aux[i] = arr[i];
  }
  let i = l, j = mid + 1;
  for (let k = l; k <= r; k++) {
    if (i > mid) {
      arr[k] = aux[j++];
    } else if (j > r) {
      arr[k] = aux[i++];
    } else if (aux[i] < aux[j]) {
      arr[k] = aux[i++];
    } else {
      arr[k] = aux[j++];
    }
  }
}

function mergerSort(arr) {

  const n = arr.length - 1;
  const aux = new Array(n + 1);

  const l = 0;

  let mid = 0;
  let r = mid + 1;

  // 有序数组1:[l...mid]
  // 有序数组2：[mid+1,r]

  while (r < n) {
    while (arr[mid] <= arr[mid + 1]) {
      mid++;
    }
    r = mid + 1;
    while (arr[r] <= arr[r + 1]) {
      r++;
    }
    merge(arr, aux, l, mid, r);
  }

  assert(isSorted(arr));
}

let n = 1000;
let last = 0;
while (true) {
  const arr = [];
  for (let i = 0; i < n; i++) {
    arr.push(Math.random());
  }
  const start = Date.now();
  mergerSort(arr);
  const end = Date.now();
  const time = end - start;
  const rate = time / last;
  last = time;
  console.log('n = ', n, 'time = ', time, 'rate = ', rate);
  n *= 2;
}

// n =  1000 time =  5 rate =  Infinity
// n =  2000 time =  6 rate =  1.2
// n =  4000 time =  26 rate =  4.333333333333333
// n =  8000 time =  72 rate =  2.769230769230769
// n =  16000 time =  288 rate =  4
// n =  32000 time =  1158 rate =  4.020833333333333
// n =  64000 time =  4593 rate =  3.966321243523316
// n =  128000 time =  18187 rate =  3.959721315044633
// n =  256000 time =  75830 rate =  4.169461703414527
