// 子数组长度

//

const counter = {};

function _merge(arr, aux, start, mid, end) {
  const n = end - start + 1;
  counter[n] = counter[n] || [];
  for (let k = start;k <= end;k++) {
    aux[k] = arr[k];
  }
  let i = start, j = mid + 1;
  let firstOutOfBounds = true;
  for (let k = start; k <= end; k++) {
    if (i > mid) {
      const restLen = end - j + 1;
      arr[k] = aux[j++];
      if (firstOutOfBounds) {
        counter[n].push(restLen);
        firstOutOfBounds = false;
      }
    } else if (j > end) {
      const restLen = mid - i + 1;
      arr[k] = aux[i++];
      if (firstOutOfBounds) {
        counter[n].push(restLen);
        firstOutOfBounds = false;
      }
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

function mergeSort(arr) {
  const aux = arr.slice();
  _mergeSort(arr, aux, 0, arr.length - 1);
}

for (let n = 1024;n < 1024*1e3;n += 1e2) {
  const arr = [];
  for (let i = 0;i < n;i++) {
    arr.push(Math.random());
  }
  mergeSort(arr);
}

const points = [];

for (let n in counter) {
  let sum = counter[n].reduce((p,c) => p + c);
  let avg = sum / counter[n].length;
  points.push([n,avg]);
}
