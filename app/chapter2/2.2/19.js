// 倒置

// 线性对数级别的算法统计给定数组中"倒置"的数量（即插入排序需要交换的次数，参见2.1节）
// 这个数量和Kendall tau距离有关，参见2.5节

function reverseCount1(arr) {
  const n = arr.length;
  let count = 0;
  for (let i = 0; i < n; i++) {
    for (let j = i + 1; j < n; j++) {
      if (arr[i] > arr[j]) {
        count++;
      }
    }
  }
  return count;
}

function merge(arr, aux, l, mid, r) {
  let inversions = 0;
  for (let k = l; k <= r; k++) {
    aux[k] = arr[k];
  }
  let i = l, j = mid + 1;
  for (let k = l; k <= r; k++) {
    if (i > mid) {
      arr[k] = aux[j++];
    } else if (j > r) {
      arr[k] = aux[i++];
    } else if (aux[j] < arr[i]) {
      arr[k] = aux[j++];
      inversions += (mid - i + 1);
    } else {
      arr[k] = aux[i++];
    }
  }
  return inversions;
}

function count(arr, aux, l, r) {
  let inversions = 0;
  if (l >= r) return inversions;
  const mid = l + parseInt((r - l) / 2);
  inversions += count(arr, aux, l, mid);
  inversions += count(arr, aux, mid + 1, r);
  inversions += merge(arr, aux, l, mid, r);
  return inversions;
}

function reverseCount2(arr) {
  const copy = arr.slice();
  const n = arr.length;
  const aux = new Array(n);
  return count(copy, aux, 0, n - 1);
}

const arr = [1, 4, 3, 2, 5];
const c = reverseCount1(arr);
const c2 = reverseCount2(arr);
debugger;
