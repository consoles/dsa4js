// 非递归的快排。
// 使用一个循环来将弹出栈的子数组切分并将结果子数组重新压入栈。注意：先将较大的子数组压入栈，这样就可以保证栈中最多只会有lgN个元素

const assert = require('assert');
const Stack = require('../../LinkedStack');
const swap = require('../../swap');
const {shuffle, isSorted} = require('../../util');
const {sortCompare, quickSort} = require('../../sort');

function partition(arr, lo, hi) {
  const v = arr[lo];
  let i = lo, j = hi + 1;
  while (true) {
    while (arr[++i] < v) if (i === hi) break;
    while (arr[--j] > v) if (j === lo) break;
    if (i >= j) break;
    swap(arr, i, j);
  }
  swap(arr, lo, j);
  return j;
}

function quickSortStack(arr) {
  shuffle(arr);
  const s = new Stack();
  s.push([0, arr.length - 1]);
  while (!s.isEmpty()) {
    const [lo, hi] = s.pop();
    if (lo >= hi) continue;
    const j = partition(arr, lo, hi);
    const item1 = [lo, j - 1];
    const item2 = [j + 1, hi];
    // 较大的子数组入栈
    if (j - lo > hi - j) {
      s.push(item1);
      s.push(item2);
    } else {
      s.push(item2);
      s.push(item1);
    }
  }
  assert(isSorted(arr));
}

let n = 1000;
while (true) {
  const ret = sortCompare(n, 10, quickSort, quickSortStack);
  console.log('n = ', n, ret);
  n *= 2;
}

// n =  1000 { quickSort: 3.4, quickSortStack: 2.7 }
// n =  2000 { quickSort: 3.4, quickSortStack: 3.6 }
// n =  4000 { quickSort: 6.9, quickSortStack: 7.1 }
// n =  8000 { quickSort: 13, quickSortStack: 13.9 }
// n =  16000 { quickSort: 28.9, quickSortStack: 30 }
// n =  32000 { quickSort: 53.4, quickSortStack: 55.7 }
// n =  64000 { quickSort: 113.1, quickSortStack: 115.8 }
// n =  128000 { quickSort: 225.1, quickSortStack: 223.9 }
// n =  256000 { quickSort: 459.5, quickSortStack: 461.8 }
// n =  512000 { quickSort: 905.1, quickSortStack: 930 }
