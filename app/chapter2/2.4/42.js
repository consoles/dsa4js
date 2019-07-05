class PreOrderHeapSort {
  constructor() {
    this.compareCount = 0;
  }

  _less(arr, i, j) {
    this.compareCount++;
    return arr[i] < arr[j];
  }

  static _swap(arr, i, j) {
    [arr[i], arr[j]] = [arr[j], arr[i]];
  }

  /**
   * 堆中的元素下沉
   * @param arr
   * @param p 需要下沉的节点的下标
   * @param n 堆中元素的数目
   */
  _sink(arr, p, n) {
    if (n <= 1) return;
    const k = parseInt(Math.log2(n));
    let left = 2 ** (k - 1) - 1;
    let right = left;
    if (n - left <= 2 ** k) {
      // 叶子节点全在左侧
      left = n - right - 1;
    } else {
      left = 2 ** k - 1;
      right = n - left - 1;
    }
    // 找出较大的子节点
    let j = p + 1, size = left;
    if (right !== 0) {
      if (this._less(arr, j, p + left + 1)) {
        j = p + left + 1;
        size = right;
      }
    }
    // 与根节点比较
    if (this._less(arr, p, j)) {
      // 交换，继续下沉
      PreOrderHeapSort._swap(arr, p, j);
      this._sink(arr, j, size);
    }
  }

  /**
   * 递归建堆
   * @param arr
   * @param p 堆的起始下标
   * @param n 堆的元素数目
   */
  buildTree(arr, p, n) {
    console.log('p = ', p, 'n = ', n);
    if (n <= 1) return;
    let k = parseInt(Math.log2(n)); // 高度
    // 去掉第k层后，根节点的左右子树的节点总数
    let left = 2 ** (k - 1) - 1;
    let right = left;
    // 去掉第k层后的节点总数为 2^0 + 2^1 + 2^2 = 2^k-1
    // 第k层如果满则节点数为2^k，取半2^(k-1)
    // 临界点 2^k - 1 + 2^(k-1)
    if (n <= 2 ** k + left) {
      // 叶子节点全在左侧
      left = n - right - 1;
    } else {
      left = 2 ** k - 1;
      right = n - left - 1;
    }
    this.buildTree(arr, p + 1, left);
    this.buildTree(arr, p + 1 + left, right);
    this._sink(arr, p, n);
  }

  sort(arr) {
    let n = arr.length;
    this.buildTree(arr, 0, n);
    // 排序
    while (n > 1) {
      const tail = this.getTail(arr, 0, n);
      const tmp = arr[tail];
      for (let i = tail + 1; i < n; i++) {
        arr[i - 1] = arr[i];
      }
      n--;
      PreOrderHeapSort._swap(arr, 0, n);
      arr[0] = tmp;
      this._sink(arr, 0, n);
    }
  }

  getTail(arr, p, n) {
    if (n <= 1) return p;
    const k = parseInt(Math.log2(n));
    let left = 2 ** (k - 1) - 1;
    let right = left;
    if (n - left <= 2 ** k) {
      left = n - right - 1;
      return this.getTail(arr, p + 1, left);
    }
    left = 2 ** k - 1;
    right = n - left - 1;
    return this.getTail(arr, p + 1 + left, right);
  }
}

const arr = [2, 1, 4, 7, 3, 5, 9, 6, 8];
const s = new PreOrderHeapSort();
s.sort(arr);
debugger;
