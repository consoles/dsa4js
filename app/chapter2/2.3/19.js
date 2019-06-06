// 五取样切分

// 实现一种基于随机抽取数组中5个元素并取中位数进行切分的快排。将取样元素放在数组的一侧用来保证只有中位数元素参与了切分。运行双倍测试来确定这项改动的效果，并和标准的快排以及三取样切分的快排进行比较。附加题：找到一种对于任意输入都只需要少于7次比较的5取样算法。

// 主要介绍一下这个少于七次比较的五取样算法。
// 首先假设五个数字为 a b c d e
// 对 b c 排序，d e 排序。（两次比较）
// 比较 b 和 d，把较小那一组换到 b c 的位置上去。（一次比较）
// 此时会有 b < c, b < d < e,b是b,c,d,e中的最小值
// 交换 a, b，重新对 b c 排序。（一次比较）
// 再次比较 b 和 d，把较小的那一组换到 b c 的位置上。（一次比较）
// 最后比较 c 和 d，较小的那一个即为中位数。（一次比较）
// 总共需要 6 次比较，严格小于 7 次。

// 取样完毕后，a b 是最小值和次小值（这里没有对应关系，a 也可以是次小值）。
// d 和 e 是最大值和次大值（同样没有对应关系）。
// 我们把 d 和 e 放到数组的最后作为哨兵，去掉右边界的判断。
// 同时让左右两侧指针都向中间移动两位，减少不必要的比较。

const assert = require('assert');

const {shuffle, isSorted} = require('../../util');
const swap = require('../../swap');
const {sortCompare, quickSort} = require('../../sort');

function partition(arr, lo, hi) {
  let i = lo, j = hi + 1;
  if (arr[lo + 1] < arr[lo]) {
    swap(arr, lo + 1, lo);
  }
  if (arr[lo + 2] < arr[lo]) {
    swap(arr, lo + 2, lo);
  }
  // lo最小,放在最前面了，接下来比较arr[lo+1]&arr[lo+2]
  if (arr[lo + 2] < arr[lo + 1]) {
    swap(arr, lo + 2, lo + 1);
  }
  // arr[lo],arr[lo+1],arr[lo+2]从小到大排列了
  swap(arr, lo, lo + 1); // 中位数放左侧
  swap(arr, hi, lo + 2); // 较大的值放在最右侧作为哨兵
  const v = arr[lo];
  while (true) {
    while (arr[++i] < v) ;
    while (arr[--j] > v) ;
    if (i >= j) break;
    swap(arr, i, j);
  }
  swap(arr, lo, j);
  return j;
}

function _quickSort(arr, lo, hi) {
  if (hi <= lo) return;
  // 两个数组的元素直接排序，因为partition寻找中位数的过程中数组长度至少为3
  if (hi === lo + 1) {
    if (arr[hi] < arr[lo]) {
      swap(arr, lo, hi);
    }
    return;
  }
  const j = partition(arr, lo, hi);
  _quickSort(arr, lo, j - 1);
  _quickSort(arr, j + 1, hi);
}

function quickSort3Partition(arr) {
  shuffle(arr);
  _quickSort(arr, 0, arr.length - 1);
  assert(isSorted(arr));
}

function quickSort5Partition(arr) {
  shuffle(arr);
  _sort(arr, 0, arr.length - 1);
  assert(isSorted(arr));
}

function _partition(arr, lo, hi) {
  // a:arr[lo],
  // b:arr[lo+1],
  // c:arr[lo+2],
  // d:arr[lo+3],
  // e:arr[lo+4]

  // 首先对 b c 排序
  if (arr[lo + 1] > arr[lo + 2]) {
    swap(arr, lo + 1, lo + 2);
  }
  // 然后再排序 d e
  if (arr[lo + 3] > arr[lo + 4]) {
    swap(arr, lo + 3, lo + 4);
  }
  // 这时满足 b < c, d < e,比较 b d，把较小的一组放到 b c 的位置上去
  if (arr[lo + 3] < arr[lo + 1]) {
    swap(arr, lo + 1, lo + 3);
    swap(arr, lo + 2, lo + 4);
  }
  // 这时满足 b < c, b < d < e, 交换 a 和 b
  swap(arr, lo, lo + 1);
  // 重新排序 b c
  if (arr[lo + 2] < arr[lo + 1]) {
    swap(arr, lo + 2, lo + 1);
  }
  // 这时再次满足 b < c, d < e,比较 b d，把最小的一组放到 b c 的位置上去
  if (arr[lo + 3] < arr[lo + 1]) {
    swap(arr, lo + 1, lo + 3);
    swap(arr, lo + 2, lo + 4);
  }
  // 这时 a 和 b 为五个数中的最小值和次小值（顺序不固定，a 也可以是次小值）,d e为5个数中的最大值和次大只
  // 最后比较 c 和 d，较小的那一个即为中位数（即第三小的数）
  if (arr[lo + 3] < arr[lo + 2]) {
    swap(arr, lo + 3, lo + 2);
  }
  swap(arr, lo + 2, lo);
  // d e 放到数组末尾充当哨兵
  swap(arr, lo + 3, hi);
  swap(arr, lo + 4, hi - 1);

  // 调整指针位置，前2位和2两位已经在何时的位置了
  let i = lo, j = hi + 1;
  i += 2;
  j -= 2;
  const v = arr[lo];
  while (true) {
    while (arr[++i] < v) ;
    while (arr[--j] > v) ;
    if (i >= j) break;
    swap(arr, i, j);
  }
  swap(arr, lo, j);
  return j;
}

function _sort(arr, lo, hi) {
  if (hi <= lo) return;
  const len = hi - lo + 1;
  // 少于5个元素直接进行插入排序
  if (len < 5) {
    for (let i = lo; i <= hi; i++) {
      for (let j = i; j > 0 && arr[j] < arr[j - 1]; j--) {
        swap(arr, j, j - 1);
      }
    }
    return;
  }
  const j = _partition(arr, lo, hi);
  _sort(arr, lo, j - 1);
  _sort(arr, j + 1, hi);
}

let n = 1000;
while (true) {
  const ret = sortCompare(n, 10, quickSort, quickSort3Partition, quickSort5Partition);
  console.log('n = ', n, JSON.stringify(ret));
  n *= 2;
}

// n =  1000 {"quickSort":3.8,"quickSort3Partition":1.9,"quickSort5Partition":2}
// n =  2000 {"quickSort":3.8,"quickSort3Partition":3.5,"quickSort5Partition":3.2}
// n =  4000 {"quickSort":7.2,"quickSort3Partition":7.1,"quickSort5Partition":7}
// n =  8000 {"quickSort":12.7,"quickSort3Partition":13.7,"quickSort5Partition":13.5}
// n =  16000 {"quickSort":29,"quickSort3Partition":30.4,"quickSort5Partition":27.2}
// n =  32000 {"quickSort":57.7,"quickSort3Partition":56.7,"quickSort5Partition":55.5}
// n =  64000 {"quickSort":112.3,"quickSort3Partition":112.8,"quickSort5Partition":111.1}
// n =  128000 {"quickSort":226.7,"quickSort3Partition":222.4,"quickSort5Partition":232.1}
// n =  256000 {"quickSort":462.3,"quickSort3Partition":453.9,"quickSort5Partition":462.9}
// n =  512000 {"quickSort":917.3,"quickSort3Partition":926.2,"quickSort5Partition":912.6}
