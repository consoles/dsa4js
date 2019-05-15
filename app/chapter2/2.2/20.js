// 间接排序

// 不改变数组的归并排序，返回一个int[]数组perm，其中perm[i]为原数组中第i小的元素的位置

function mergeSortIndex(arr) {
  const n = arr.length;
  const indexes = [];
  for (let i = 0;i < n;i++) {
    indexes.push(i);
  }
  const aux = new Array(n);
  sortIndex(arr, indexes, aux, 0, n - 1);
  return indexes;
}

function merge(arr, indexes, aux, l, mid, r) {
  for (let i = l; i <= r; i++) {
    aux[i] = indexes[i];
  }
  let i = l, j = mid + 1;
  for (let k = l; k <= r; k++) {
    if (i > mid) {
      indexes[k] = aux[k++];
    } else if (j > r) {
      indexes[k] = aux[i++];
    } else if (arr[aux[i]] < arr[aux[j]]) {
      indexes[k] = aux[i++];
    } else {
      indexes[k] = aux[j++];
    }
  }
}

function sortIndex(arr, indexes, aux, l, r) {
  if (l >= r) return;
  const mid = l + Math.floor((r - l) / 2);
  sortIndex(arr, indexes, aux, l, mid);
  sortIndex(arr, indexes, aux, mid + 1, r);
  merge(arr, indexes, aux, l, mid, r);
}

const arr = [2,1,3,5,4];
const ret = mergeSortIndex(arr);
debugger;
