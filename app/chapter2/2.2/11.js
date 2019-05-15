// 改进

// 实现2.2.2节的对归并排序的3项改进：
// 1.加快小数组的排序速度
// 2.检测数组是否已经有序
// 3.通过在递归中交换参数来避免数组复制

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
  if (end - start <= 2) {
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

const arr = [1, 4, 3, 2, 5, 7, 6];
mergeSort(arr);
debugger;
