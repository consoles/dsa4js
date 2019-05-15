// 创建数组

// 使用SortCompare粗略比较在merge中和在sort中创建aux[]的性能差异

function _merge(arr, aux, start, mid, end) {
  for (let k = start;k <= end;k++) {
    aux[k] = arr[k];
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

function _mergeSort(arr, aux, start, end) {
  if (start >= end) return;

  const mid = start + parseInt((end - start) / 2);
  _mergeSort(arr, aux, start, mid);
  _mergeSort(arr, aux, mid + 1, end);
  _merge(arr, aux, start, mid, end);
}

function mergeSort1(arr) {
  const aux = arr.slice();
  _mergeSort(arr, aux, 0, arr.length - 1);
}

function _merge2(arr,start,mid,end) {
  const aux = [];
  let i = start,j = mid+1;
  for (let k = start;k <= end;k++) {
    if (i > mid) {
      aux.push(arr[j++]);
    } else if (j > end) {
      aux.push(arr[i++]);
    } else if (arr[i] < arr[j]) {
      aux.push(arr[i++]);
    } else {
      aux.push(arr[j++]);
    }
  }
  for (let k = start;k <= end;k++) {
    arr[k] = aux[k - start];
  }
}

function _mergeSort2(arr,start,end) {
  if (start >= end) return;

  const mid = start + parseInt((end - start) / 2);
  _mergeSort2(arr, start, mid);
  _mergeSort2(arr, mid + 1, end);
  _merge2(arr, start, mid, end);
}

function mergeSort2(arr) {
    _mergeSort2(arr,0,arr.length - 1);
}

const genRandomArr = N => {
  const arr = [];
  for (let i = 0; i < N; i++) {
    arr.push(Math.random());
  }
  return arr;
};

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

    const t1 = time(mergeSort1, T, n);
    const t2 = time(mergeSort2, T, n);

    console.log(`T = ${T},N = ${n}`);
    console.log('重用数组', t1);
    console.log('创建新数组', t2);
    console.log('创建新数组 / 重用数组', t2 / t1);
    console.log();
  }
})();

// T = 10,N = 1024
// 重用数组 1
// 创建新数组 1
// 创建新数组 / 重用数组 1
//
// T = 10,N = 2048
// 重用数组 0.3
// 创建新数组 0.4
// 创建新数组 / 重用数组 1.3333333333333335
//
// T = 10,N = 4096
// 重用数组 0.5
// 创建新数组 0.8
// 创建新数组 / 重用数组 1.6
//
// T = 10,N = 8192
// 重用数组 1
// 创建新数组 1.9
// 创建新数组 / 重用数组 1.9
//
// T = 10,N = 16384
// 重用数组 2.9
// 创建新数组 3.9
// 创建新数组 / 重用数组 1.3448275862068966
//
// T = 10,N = 32768
// 重用数组 5.5
// 创建新数组 7.3
// 创建新数组 / 重用数组 1.3272727272727272
//
// T = 10,N = 65536
// 重用数组 12
// 创建新数组 15.8
// 创建新数组 / 重用数组 1.3166666666666667
//
// T = 10,N = 131072
// 重用数组 25
// 创建新数组 33.6
// 创建新数组 / 重用数组 1.344
//
// T = 10,N = 262144
// 重用数组 50.7
// 创建新数组 69.4
// 创建新数组 / 重用数组 1.368836291913215
//
// T = 10,N = 524288
// 重用数组 106.3
// 创建新数组 155.7
// 创建新数组 / 重用数组 1.4647224835371588
//
// T = 10,N = 1048576
// 重用数组 229.3
// 创建新数组 338.3
// 创建新数组 / 重用数组 1.4753597906672482
//
// T = 10,N = 2097152
// 重用数组 473.6
// 创建新数组 756.5
// 创建新数组 / 重用数组 1.597339527027027
