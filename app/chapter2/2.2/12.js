// 次线性的额外空间

// 用大小M将数组分为N/M块（简单起见，设M是N的约数）。
// 实现一个归并方法，使得所需要的额外空间减少到max(M,N/M)
// 1. 可以先将一个块看做一个元素，将块的第一个元素作为块的主键，用选择排序将块排序
// 2. 遍历数组，将第一块和第二块归并，完成后将第二块和第三块归并，等等

function selectionSort(arr, l, r) {
  for (let i = l; i <= r; i++) {
    let minIndex = i;
    for (let j = i + 1; j <= r; j++) {
      if (arr[j] < arr[minIndex]) {
        minIndex = j;
      }
    }
    if (minIndex !== i) {
      [arr[i], arr[minIndex]] = [arr[minIndex], arr[i]];
    }
  }
}

function merge(arr, aux, start, mid, end) {
  for (let k = start; k <= end; k++) {
    aux[k - start] = arr[k];
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

function sort(arr) {
  const M = 3;
  const N = arr.length;

  const count = parseInt(N / M);

  // 对每一块进行选择排序
  let rightOverBounds = false;
  for (let l = 0; l < N;) {
    let r = l + M - 1;
    if (r >= N) {
      r = N - 1;
      rightOverBounds = true;
    }
    selectionSort(arr, l, r);
    if (rightOverBounds) {
      break;
    }
    l += M;
  }

  const sz = Math.max(count, M);
  const aux = new Array(sz);

  const start = 0;
  for (let i = 0; i < count; i++) {
    const mid = (i + 1) * M - 1;
    let end = mid + M;
    if (end > N - 1) {
      end = N - 1;
    }
    merge(arr, aux, start, mid, end);
  }
}

const arr = [2, 1, 4, 3, 5, 7, 4, 2, 1, 3, 6, 7, -1, -2];
sort(arr);
debugger;
