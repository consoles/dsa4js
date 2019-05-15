// 改进

// 用实验评估正文中提到了归并排序的三项改进（练习2.2.11）的效果，并比较正文中实现的归并和练习2.2.10所实现的归并排序之间的性能。根据姜堰给出应该在何时为子数组切换到插入排序
function insertSort(arr, l, r) {
  for (let i = l; i <= r; i++) {
    for (let j = i + 1; j > 0 && arr[j] < arr[j - 1]; j--) {
      [arr[j], arr[j - 1]] = [arr[j - 1], arr[j]];
    }
  }
}

function merge(arr, aux, start, mid, end) {
  let i = start, j = mid + 1;
  for (let k = start; k <= end; k++) {
    if (i > mid) {
      arr[k] = aux[j++];
    } else if (j > end) {
      arr[k] = aux[i++];
    } else if (aux[i] < aux[j]) {
      arr[k] = aux[i++];
    } else {
      arr[k] = aux[j++];
    }
  }
}

function _mergeSort(arr, aux, start, end) {
  if (start >= end) return;
  if (end - start <= 8) {
    // 针对小数组采用插入排序
    insertSort(arr, start, end);
    return;
  }

  const mid = start + parseInt((end - start) / 2);

  // 在每个层次，交换输入数组和辅助数组的角色
  _mergeSort(aux, arr, start, mid);
  _mergeSort(aux, arr, mid + 1, end);

  // 前后两半部分已经有序，无需merge
  if (aux[mid] <= aux[mid + 1]) {
    // copy
    for (let i = start; i <= end; i++) {
      arr[i] = aux[i];
    }
    return;
  }
  merge(arr, aux, start, mid, end);
}

function mergeSort(arr) {
  const aux = arr.slice();
  _mergeSort(arr, aux, 0, arr.length - 1);
}

function mergeSort2(arr) {
  const aux = arr.slice();
  _mergeSort2(arr, aux, 0, arr.length - 1);
}

function merge2(arr, aux, start, mid, end) {
  for (let i = start; i <= end; i++) {
    aux[i] = arr[i];
  }
  let i = start, j = mid + 1;
  for (let k = start; k <= end; k++) {
    if (i > mid) {
      arr[k] = aux[j++];
    } else if (j > end) {
      arr[k] = aux[i++];
    } else if (aux[i] < aux[j]) {
      arr[k] = aux[i++];
    } else {
      arr[k] = aux[j++];
    }
  }
}

function _mergeSort2(arr, aux, start, end) {
  if (start >= end) return;

  const mid = start + parseInt((end - start) / 2);

  // 在每个层次，交换输入数组和辅助数组的角色
  _mergeSort2(arr, aux, start, mid);
  _mergeSort2(arr, aux, mid + 1, end);

  merge2(arr, aux, start, mid, end);
}

const genRandomArr = N => {
  const arr = [];
  for (let i = 0; i < N; i++) {
    arr.push(Math.random());
  }
  return arr;
};

/**
 * @param fn 排序函数
 * @param {*} T 重复试验的次数，多次试验取平均值可以消除偶然误差
 * @param {*} N 数组长度
 */
const time = (fn, T, N) => {
  let total = 0;
  for (let i = 0; i < T; i++) {
    const arr = genRandomArr(N);
    const start = Date.now();
    fn.call(null, arr);
    total += (Date.now() - start);
  }
  return total / T;
};

(() => {
  const T = 10;

  for (let n = 1024; true; n += n) {

    const t1 = time(mergeSort, T, n);
    const t2 = time(mergeSort2, T, n);

    console.log(`T = ${T},N = ${n}`);
    console.log('原始归并', t1);
    console.log('优化后的归并', t2);
    console.log('优化前 / 优化后', t1 / t2);
    console.log();
  }
})();

// T = 10,N = 1024
// 原始归并 10.6
// 优化后的归并 0.4
// 优化前 / 优化后 26.499999999999996
//
// T = 10,N = 2048
// 原始归并 33.8
// 优化后的归并 0.4
// 优化前 / 优化后 84.49999999999999
//
// T = 10,N = 4096
// 原始归并 135.2
// 优化后的归并 0.6
// 优化前 / 优化后 225.33333333333331
//
// T = 10,N = 8192
// 原始归并 515
// 优化后的归并 1
// 优化前 / 优化后 515
//
// T = 10,N = 16384
// 原始归并 2232.7
// 优化后的归并 2.7
// 优化前 / 优化后 826.9259259259258
//
// T = 10,N = 32768
// 原始归并 8165.8
// 优化后的归并 5.2
// 优化前 / 优化后 1570.3461538461538
