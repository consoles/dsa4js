# 归并排序

分为自顶向下的归并排序和自底向上的归并排序。将大数组划分成只有一个元素的数组，然后逐个比较，进行合并。典型的分治法。[分治法](http://blog.jobbole.com/100349/)

归并排序的2个优化：

1. 测试数组是否有序。如果`arr[mid] < arr[mid+1]`我们就认为数组已经有序，从而可以跳过`merge()`方法
2. 不将元素复制到辅助数组。

自底向上的归并排序思路是：

1. 两两归并（两个长度为 1 的数组合并成一个长度为 2 的数组）
2. 四四归并（两个长度为 2 的数组合并成一个长度为 4 的数组）
3. 八八归并

```js
/**
 * 将[start,mid],[mid+1,end]的数组归并，形成一个大的有序数组arr[start...end]
 */
const merge = (arr, start, mid, end) => {
	const aux = [];
	let i = start;
	let j = mid + 1;
	for (let k = start; k <= end; k++) {
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
	for (let i = 0; i < aux.length; i++) {
		arr[start + i] = aux[i];
	}
}

/**
 * 自底向上的归并排序
 */
const mergeSortBottomUp = arr => {
	const n = arr.length;
	for (let sz = 1; sz < n; sz *= 2) {
		for (let start = 0; start < n - sz; start += 2 * sz) {
			// 归并 arr[start...start+2*sz)
			const left = start;
			const right = Math.min(start + 2 * sz - 1, n - 1);// 防止右侧越界
			const mid = start + sz - 1;
			merge(arr, left, mid, right);
		}
	}
};
```

自底向上的归并排序会多次遍历整个数组，根据子数组的大小进行两两归并。最后一个子数组的大小只有在数组大小是sz偶数倍的时候才会等于sz(否则它会比sz小)。

自底向上的归并排序和自顶向下的归并排序具有相同的复杂度。这两种方式体现了两种算法思想：

- 自顶向下：化整为零的方式解决问题（然后递归地解决它们）
- 自底向上：循序渐进地解决问题

自顶向下的归并排序可以将数组比较均匀地划分，而自底向上的归并排序则最后一段数组的划分没有那么均匀（后面一段数组可能较小），自顶向下的归排序需要使用递归有额外的函数调用开销，因此虽然自底向上的归并排序数组划分没那么均匀，但是性能反而更优。

上面的代码在 merge 操作的时候需要判断左右边界，如果按照降序将 arr 的后半部分复制到 aux 数组可以省略边界判断，这样排序的结果将是*不稳定*的:

```javascript
class Merger {
  constructor(arr) {
    this.arr = arr;
    this.aux = new Array(arr.length);
  }

  merge(start, mid, end) {
    for (let k = start; k <= mid; k++) {
      this.aux[k] = this.arr[k];
    }
    const offset = mid + 1;
    for (let k = offset; k <= end; k++) {
      this.aux[k] = this.arr[end - k + offset];
    }
    let i = start, j = end;
    for (let k = start; k <= end; k++) {
      if (this.aux[i] < this.aux[j]) {
        this.arr[k] = this.aux[i++];
      } else {
        this.arr[k] = this.aux[j--];
      }
    }
  }
}

class MergeSortBottomUp extends Merger {
  constructor(arr) {
    super(arr);
  }

  sort() {
    const n = this.arr.length;
    for (let sz = 1; sz < n; sz *= 2) {
      for (let start = 0; start + sz < n; start += 2 * sz) {
        const mid = start + sz - 1;
        const end = Math.min(n - 1, mid + sz);
        this.merge(start, mid, end);
      }
    }
  }
}

class MergeSortTopDown extends Merger {
  constructor(arr) {
    super(arr);
  }

  sort() {
    this._mergeSort(0, this.arr.length - 1);
  }

  _mergeSort(start, end) {
    if (start >= end) return;
    const mid = start + parseInt((end - start) / 2);
    this._mergeSort(start, mid);
    this._mergeSort(mid + 1, end);
    this.merge(start, mid, end);
  }
}
```

归并排序的优化：

```javascript
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
  if (arr[mid] <= arr[mid + 1]) {
    return;
  }
  
  merge(arr, aux, start, mid, end);
}

function mergeSort(arr) {
  const aux = arr.slice();
  _mergeSort(arr, aux, 0, arr.length - 1);
}
```

## 自然的归并排序

```javascript
// 自然的归并排序

// 编写一个自底向上的归并排序，当需要将2个子数组排序时能够利用数组中已经有序的部分
// 首先找到一个有序的子数组（移动指针直到当前元素比上一个元素小为止），然后再找出另一个将它们归并
// 根据数组大小和数组中递增子数组的最大长度分析算法的运行时间

function merge(arr, aux, l, mid, r) {
  for (let i = l; i <= r; i++) {
    aux[i] = arr[i];
  }
  let i = l, j = mid + 1;
  for (let k = l; k <= r; k++) {
    if (i > mid) {
      arr[k] = aux[j++];
    } else if (j > r) {
      arr[k] = aux[i++];
    } else if (aux[i] < aux[j]) {
      arr[k] = aux[i++];
    } else {
      arr[k] = aux[j++];
    }
  }
}

function mergerSort(arr) {

  const n = arr.length - 1;
  const aux = new Array(n + 1);

  const l = 0;

  let mid = 0;
  let r = mid + 1;

  // 有序数组1:[l...mid]
  // 有序数组2：[mid+1,r]

  while (r < n) {
    while (arr[mid] <= arr[mid + 1]) {
      mid++;
    }
    r = mid + 1;
    while (arr[r] <= arr[r + 1]) {
      r++;
    }
    merge(arr, aux, l, mid, r);
  }
}
```

## 归并排序思想的应用

### 利用归并排序的思想求解数组中逆序对的数量

```javascript
// 比较容易想到的是暴力法
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

// 归并排序的时候统计aux[j] < aux[i]的次数
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
      // [i,mid]是有序区,arr[i] > arr[j]，所以这个区间内的所有元素都大于arr[j]
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
  // 复制，count会改变数组
  const copy = arr.slice();
  const n = arr.length;
  const aux = new Array(n);
  return count(copy, aux, 0, n - 1);
}
```

没有任何*基于比较*的排序算法能够保证使用少于`lg(N!)~N*lgN`次的比较将长度为N的数组排序。归并排序是一种渐进最优的基于比较的排序算法。

### 利用归并排序的思想排序链表

```javascript
function mergeSortedList(list1, list2) {
  const dummyHead = new Node(-1, list1); // 引入list1的辅助头结点 dummyHead，因为可能在头部插入
  let cur1 = dummyHead,
    cur2 = list2;

  while (cur1.next && cur2) {
    if (cur1.next.value > cur2.value) {
      list2 = cur2.next;
      cur2.next = cur1.next;
      cur1.next = cur2;
      cur1 = cur2;
      cur2 = list2;
    } else {
      cur1 = cur1.next;
    }
  }
  if (!cur1.next) {
    cur1.next = cur2;
  }
  return dummyHead.next;
}

function sortLinkedList(head) {
  if (!head || !head.next) return head;

  // 使用快慢指针寻找中间节点的位置
  let slow = head,
    fast = head;

  while (fast.next && fast.next.next) {
    fast = fast.next.next;
    slow = slow.next;
  }

  let leftHead = head,
    rightHead = slow.next;

  slow.next = null; // 左半部分链表的next域清空，先用一个变量记录右边链表的头

  leftHead = sortLinkedList(leftHead);
  rightHead = sortLinkedList(rightHead);

  return mergeSortedList(leftHead, rightHead);
}
```

### 利用归并排序的思想打乱链表

只需要在上述排序链表中merge的时候的比较条件改为随机判断：

```javascript
function mergeList(list1, list2) {
  const dummyHead = new Node(-1, list1); // 引入list1的辅助头结点 dummyHead，因为可能在头部插入
  let cur1 = dummyHead,
    cur2 = list2;

  while (cur1.next && cur2) {
    if (Math.random() > .5) {
      list2 = cur2.next;
      cur2.next = cur1.next;
      cur1.next = cur2;
      cur1 = cur2;
      cur2 = list2;
    } else {
      cur1 = cur1.next;
    }
  }
  if (!cur1.next) {
    cur1.next = cur2;
  }
  return dummyHead.next;
}
```

### 利用归并排序的思想进行索引排序

不改变数组的归并排序，返回一个数组arr,其中arr[i]的值是源数组中第i小元素的位置

类似索引二叉堆，使用索引间接查找元素

```javascript
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
```

## 归并排序的其他实现

### 三路归并

```javascript
function mergeSort3Ways(arr) {
  const n = arr.length;
  const aux = new Array(n);
  sort(arr, aux, 0, n - 1);
}

// merge的时候需要多考虑几种清空，这里利用flag的求和，表示状态，可以减少判断条件的个数
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
```

### K路归并排序

```javascript
function _mergeSortKWays(arr, aux, start, end, k) {
  if (start >= end) return;
  let startNew = start;
  let endNew = 0;
  const step = parseInt((end - start) / k);
  for (let i = 0; i < k; i++) {
    endNew = startNew + step;
    if (endNew > end) {
      endNew = end;
    }
    _mergeSortKWays(arr, aux, startNew, endNew, k);
    startNew = endNew + 1;
    if (endNew === end) {
      break;
    }
  }
  _mergeKWays(arr, aux, start, end, k);
}

function _mergeKWays(arr, aux, start, end, k) {
  for (let i = start; i <= end; i++) {
    aux[i] = arr[i];
  }
  const bounds = []; // 记录每个区间的左右端点
  let startNew = start;
  let endNew = 0;
  const step = parseInt((end - start) / k);
  for (let i = 0; i < k; i++) {
    endNew = startNew + step;
    if (endNew > end) {
      endNew = end;
    }
    bounds.push([startNew, endNew]);
    startNew = endNew + 1;
    if (endNew === end) {
      break;
    }
  }
  let minIndex = 0;
  for (let i = start; i <= end; i++) {
    for (let j = 0; j < bounds.length; j++) {
      if (bounds[j][0] <= bounds[j][1] && bounds[j][0] <= end) {
        // 一个保底的minIndex
        minIndex = j;
        break;
      }
    }
    for (let j = 0; j < bounds.length; j++) {
      if (bounds[j][0] <= bounds[j][1] && bounds[j][0] <= end) {
        // 依次比较每个区间的开头的元素
        if (aux[bounds[j][0]] < aux[bounds[minIndex][0]]) {
          minIndex = j;
        }
      }
    }
    arr[i] = aux[bounds[minIndex][0]];
    bounds[minIndex][0]++; // ?? 有待琢磨 
  }
}

function mergeSortKWays(arr, k) {
  const n = arr.length;
  const aux = new Array(n);
  _mergeSortKWays(arr, aux, 0, n - 1, k);
}
```
