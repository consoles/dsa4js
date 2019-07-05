// 线性时间判断数组时候是最小堆

function isMinHeap(pq) {
  // 左孩子2k,右孩子2k+1
  const n = pq.length;
  for (let k = 1; k <= n; k++) {
    const l = 2 * k;
    if (l >= n) {
      return true;
    }
    if (pq[k] > pq[l]) return false;
    if (l + 1 < n && pq[k] > pq[l + 1]) return false;
  }
  return true;
}

function isMinHeap2(pq, k) {
  const n = pq.length;
  if (k > n) return true;
  const left = 2 * k;
  const right = left + 1;
  if (left <= n && pq[k] > pq[left]) return false;
  if (right <= n && pq[k] > pq[right]) return false;
  return isMinHeap2(pq, left) && isMinHeap2(pq, right);
}

const arr = [-1, 1, 2, 3, 4, 5, 6, 7];
const flag = isMinHeap2(arr, 1);
debugger;
