// 三路归并排序

// 假设每次将数组分为3个部分而不是2个部分并将它们分别排序，然后进行三路归并。这种算法的运行时间增长数量级是多少

const assert = require('assert');

const {isSorted} = require('../../util');

function mergeSort3Ways(arr) {
  const n = arr.length;
  const aux = new Array(n);
  sort(arr, aux, 0, n - 1);
  if (!isSorted(arr)) {
    console.log(arr);
    throw 123;
  }
  assert(isSorted(arr));
}

function merge(arr, aux, l, lmid, rmid, r) {
  for (let i = l; i <= r; i++) {
    aux[i] = arr[i];
  }
  let i = l,
    j = lmid + 1,
    k = rmid + 1;

  for (let m = l; m <= r; m++) {
    let flag = 0; // 3个数组都没有到达最右边界（0，1，10，11，100，101，110，111）
    // 注意不能用else-if
    if (i > lmid) {
      flag += 1; // 第一个数组突破右边界
    }
    if (j > rmid) {
      flag += 10; // 第二个数组突破右边界
    }
    if (k > r) {
      flag += 100; // 第三个数组突破右边界
    }
    let min = 0;
    switch (flag) {
      case 0:
        // 三个数组都没用完
        min = Math.min.call(null, aux[i], aux[j], aux[k]);
        if (min === aux[i]) {
          arr[m] = aux[i++];
        } else if (min === aux[j]) {
          arr[m] = aux[j++];
        } else {
          arr[m] = aux[k++];
        }
        break;
      case 1:
        // 只有第一个数组用完了
        if (aux[j] < aux[k]) {
          arr[m] = aux[j++];
        } else {
          arr[m] = aux[k++];
        }
        break;
      case 10:
        // 只有第二个数组用完
        if (aux[i] < aux[k]) {
          arr[m] = aux[i++];
        } else {
          arr[m] = aux[k++];
        }
        break;
      case 100:
        // 只有第三个数组用完
        if (aux[i] < aux[j]) {
          arr[m] = aux[i++];
        } else {
          arr[m] = aux[j++];
        }
        break;
      case 11:
        // 第1，2 数组用完
        arr[m] = aux[k++];
        break;
      case 101:
        // 第1，3 数组用完
        arr[m] = aux[j++];
        break;
      case 110:
        // 第2，3 数组用完
        arr[m] = aux[i++];
        break;
      default:
        break;
    }
  }
}

function sort(arr, aux, l, r) {
  const len = r - l;
  if (len <= 0) return;
  const lmid = l + Math.floor(len / 3);
  const rmid = r - Math.floor(len / 3);
  sort(arr, aux, l, lmid);
  sort(arr, aux, lmid + 1, rmid);
  sort(arr, aux, rmid + 1, r);
  merge(arr, aux, l, lmid, rmid, r);
}

// const arr = [1,3,2,5,4,9,7,8,-1,12,10,77];
// mergeSort3Ways(arr);
// debugger;

let n = 1024;
let lastTime = 0;
while (true) {
  const arr = [];
  for (let i = 0; i < n; i++) {
    arr.push(Math.random());
  }
  const start = Date.now();
  mergeSort3Ways(arr);
  const time = Date.now() - start;
  console.log('n = ', n, 'time', time, 'rate', time / lastTime);
  lastTime = time;
  n *= 2;
}

// n =  1024 time 5 rate Infinity
// n =  2048 time 3 rate 0.6
// n =  4096 time 1 rate 0.3333333333333333
// n =  8192 time 2 rate 2
// n =  16384 time 3 rate 1.5
// n =  32768 time 6 rate 2
// n =  65536 time 12 rate 2
// n =  131072 time 28 rate 2.3333333333333335
// n =  262144 time 54 rate 1.9285714285714286
// n =  524288 time 112 rate 2.074074074074074
// n =  1048576 time 244 rate 2.1785714285714284
// n =  2097152 time 478 rate 1.959016393442623
// n =  4194304 time 1053 rate 2.202928870292887
// n =  8388608 time 2199 rate 2.0883190883190883
// n =  16777216 time 4590 rate 2.08731241473397
// n =  33554432 time 9614 rate 2.094553376906318
