## dsa4js

> 常见算法的javascript实现

### Usage

```bash
$ git clone https://github.com/consoles/dsa4js.git
$ cd dsa4js && npm install
$ mocha
```

[各种排序算法](http://wwwlgis.informatik.uni-kl.de/archiv/wwwdvs.informatik.uni-kl.de/courses/DBSREAL/SS2005/Vorlesungsunterlagen/Implementing_Sorting.pdf)

参见sort.js

### 选择排序

找到数组中的最小的那个元素，其次将它和数组的第一个元素进行交换（如果第一个元素本身就是最小的元素，那么它将和自己进行交换）。再次，在剩下的元素中找到最小的元素，将它和数组的第二个元素交换位置。如此反复，直到整个数组有序。核心思想：

> 不断在剩余元素中找到最小者。当前的索引将数组分成了2部分，左边是有序数组，右边是待排序。

大约需要N^2/2次比较和N次交换

总体来说：

- 它是一种*输入无关*的算法。为了找出最小元素而扫描一遍数组并不能为下一遍扫描提供什么信息。也就是说：一个完全有序的数组和顺序混乱的数组没有任何区别！其他排序算法更加善于利用输入的初始状态。
- *交换的次数最少*。交换的次数和数组的规模线性相关。

```js
// 比较的次数为(n-1-0) + (n-1-1) + (n-1-2) + ... + (n-1-(n-1))
// n(n-1) - n(n-1) / 2 = n(n-1) / 2
let selectionSort = arr => {
	for (let i = 0; i < arr.length; i++) {
		let minIndex = i
		// 比较的次数为n-1-i次
		for (let j = i + 1; j < arr.length; j++)
			if (arr[j] < arr[minIndex])
				minIndex = j
		// swap函数调用了n次（0~n-1）		
		swap(arr, i, minIndex)
	}
	return arr
}
```

### 插入排序

> 生活中一个形象的例子是整理桥牌：一张一张地来将每一张牌插入到其他已经有序的牌中的适当位置。在计算机的实现中为了给更小的元素腾出空间，我们需要将其余所有的元素都向右移动一位。

和选择排序类似，当前索引将数组分成了有序和无序两部分，但是有序数组的最终位置还不确定，为了给更小的元素腾出空间，它们可能被移动。当索引到达数组的最右边的时候，整个数组就有序了。核心思想：

> 不断将元素插入到已经有序的数组的适当位置。

- 效率严重取决于*数组的初始值*。一个很大的有序（或接近有序）数组的效率将比随机的数组或者逆序数组快得多
- 插入排序非常适合于处理数组中只有几个元素的位置不正确的情况。*当倒置的元素很小的时候可能是最好的算法*。

```js
// 对于1~N-1之间的每一个i，将a[i]和a[0]~a[i-1]中比它小的元素依次有序交换
// 在索引i从左向右的过程中，它左侧的元素总是有序的，所以当i到达最右端的时候排序自然完成了
let insertSort = arr => {
	for (let i = 1; i < arr.length; i++)
		// insert a[i] into arr[i-1],arr[i-2],arr[i-3],...,arr[0]
		for (let j = i; j > 0 && arr[j] < arr[j - 1]; j--)
			swap(arr, j, j - 1)
	return arr
}
```

将内循环中较大元素都向右移动而不是交换两个元素可以将数组访问的次数减半从而提升速度。

对于随机排列的数组，平均需要N^2/4次比较N^2/4次交换。最坏情况下需要N^2/2次比较和N^2/2次交换，最好情况下需要N-1次比较和0次交换。（平均情况下每个元素需要地洞半个数组的长度）

逆序对的数量可以衡量数组的有序程度，如果逆序对的数量小于数组大小的某个倍数，我们就称为这个数组是部分有序的。参见reverse-pair.js。典型的部分有序数组：

1. 数组中每个元素距离它最终的位置都不远
2. 一个有序的大数组接一个小数组
3. 数组中只有几个元素的位置不正确

插入排序实际上是依次减少逆序对的过程。

插入排序对上面的数组非常高效。

> 如果数组逆序，则选择排序的性能高于插入排序，因为插入排序比较和交换的次数都是N(N-1) / 2，选择排序的比较次数为N(N-1) / 2，交换次数为N。

#### 无需交换的插入排序

```js
// https://algs4.cs.princeton.edu/21elementary/InsertionX.java.html
const insertSortNoSwap = arr => {
    const n = arr.length;

    // 从后向前扫描，参考2.1.24中的哨兵，依次将最小的元素放在最开始的位置
    let exchanges = 0;
    for (let i = n - 1; i > 0; i--) {
        if (arr[i] < arr[i - 1]) {
            swap(arr, i, i - 1);
            exchanges++;
        }
    }
    // 经过上面的操作后，最小的元素就放在了开头（哨兵）
    if (exchanges === 0) return;

    for (let i = 2; i < n; i++) {
        const v = arr[i];
        let j = i;
        while (v < arr[j - 1]) {
            arr[j] = arr[j - 1];
            j--;
        }
        if (j !== i) {
            arr[j] = v;
        }
    }
};

const insertSortNoSwap2 = arr => {
    const n = arr.length;
    for (let i = 1; i < n; i++) {
        let v = arr[i];
        let j = i - 1;
        while (j >= 0 && v < arr[j]) {
            arr[j + 1] = arr[j];
            j--;
        }
        if (j !== i - 1) {
            arr[j + 1] = v;
        }
    }
};
```

### 希尔排序

希尔排序本质上是插入排序的改进。是一种*缩小增量排序*。考虑一种极端情形：最小的元素位于数组的尽头，那么它挪动到正确的位置需要进行n-1次移动。它交换不相邻的元素以对数组的局部进行排序，最终利用插入排序将局部有序的数组排序。

> 先将整个待排元素序列分割成若干个子序列（由相隔某个“增量”的元素组成的）分别进行直接插入排序，然后依次缩减增量再进行排序，待整个序列中的元素基本有序（增量足够小）时，再对全体元素进行一次直接插入排序。因为*直接插入排序在元素基本有序的情况下（接近最好情况），效率是很高的*，因此希尔排序在时间效率上比前两种方法有较大提高,*数组越大，优势越大*。

希尔排序更高效的原因是权衡了子数组的规模和有序性。排序之初：各个数组都很短，排序之后子数组都是部分有序的，这两种情况都非常适合插入排序。

[希尔排序](http://blog.csdn.net/morewindows/article/details/6668714)

### 归并排序

分为自顶向下的归并排序和自底向上的归并排序。
将大数组划分成只有一个元素的数组，然后逐个比较，进行合并。典型的分治法。[分治法](http://blog.jobbole.com/100349/)

归并排序的2个优化：

1. 测试数组是否有序。如果`arr[mid] < arr[mid+1]`我们就认为数组已经有序，从而可以跳过`merge()`方法
2. 不将元素复制到辅助数组。

自底向上的归并排序思路是：

1. 两两归并（两个长度为1的数组合并成一个长度为2的数组）
2. 四四归并（两个长度为2的数组合并成一个长度为4的数组）
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

上面的代码在merge操作的时候需要判断左右边界，如果按照降序将arr的后半部分复制到aux数组可以省略边界判断，这样排序的结果将是*不稳定*的:

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

#### 自然的归并排序

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

#### 利用归并排序的思想求解数组中逆序对的数量

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

#### 利用归并排序的思想排序链表

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

#### 利用归并排序的思想打乱链表

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

#### 利用归并排序的思想进行索引排序

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

#### 三路归并

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

#### K路归并排序

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

### 出列排序

问题来源：算法练习2.1.14。说说你会如何将一副扑克牌排序，限制条件是只能查看最上面的两张牌，交换最上面的两张牌，或是将最上面的一张牌放到这摞牌的最下面。

```js
const sort = arr => {
    const cards = arr.slice();
    const ret = [];
    while (true) {
        // 经过n-1次操作后最小的牌放在了最前面，将此牌出列，不断找到最小的牌
        for (let i = 0; i < cards.length - 1; i++) {
            // 将较小的一个元素向后挪一个位置，保证不会被换走
            if (cards[0] < cards[1]) {
                [cards[0], cards[1]] = [cards[1], cards[0]];
            }
            cards.push(cards.shift());
            console.log(cards);
        }
        console.log('after change', cards);
        // 最大的牌被放在了最前面(出列)
        const min = cards.shift();
        ret.push(min);
        if (cards.length === 0) {
            break;
        }
    }
    return ret;
}

const arr = [4, 3, 1, 2, 5];
const ret = sort(arr);
console.log(ret);
```

### 插入排序中的哨兵

2.1.24 插入排序的哨兵。在插入排序的实现中先找出最小的元素并将其置于数组的最左边，这样就能去掉内循环的判断条件 j > 0,这是一种常见的规避边界测试的方法，能够省略判断条件的元素通常称为哨兵。

```js
const insertSort = arr => {
    const n = arr.length;

    // 找到数组最小元素放在最左边
    let minIndex = 0;
    for (let i = 1; i < n; i++) {
        if (arr[i] < arr[minIndex]) {
            minIndex = i;
        }
    }
    if (minIndex !== 0) {
        swap(arr, 0, minIndex);
    }

    for (let i = 1; i < n; i++) {
        // 规避了判断条件j > 0
        for (let j = i; arr[j] < arr[j - 1]; j--) {
            swap(arr, j, j - 1);
        }
    }
};
```

### 背包

一种不支持从中删除元素的集合数据类型——它的目的就是帮助用例收集元素并迭代遍历所有收集到的元素（用例也可以检查背包是否为空或者背包中元素的数量）。迭代的顺序不确定且和用例无关。

### 队列

FIFO是公平性的体现。

> 南方有嘉木，谁与望天堂.

### 下压栈

栈的基本思想是LIFO。新邮件到来的时候，你总是可以看到它们在栈的最上面。这种思想的好处是我们可以及时看到最新的内容。当我们点击一个超链接的时候浏览器会进入一个新的页面（并将其压入一个栈），我们可以不断点击超链接访问新页面，但是总可以通过点击“后退”按钮重新访问以前的页面（从栈中弹出）。

#### 算术表达式求值

算术表达式可能是一个数，或者由一个左括号、一个算术表达式、一个运算符、另一个算术表达式和右括号组成的表达式。这里定义的是未省略括号的算术表达式，例如：1 + 2 * 3最该定义中应该表示为(1 + (2 * 3))。简单起见我们定义算术运算符包含二元运算加减乘除和一元运算sqrt。难点在于如何*解析*由括号、运算符和数字组成的字符串，并按照正确的顺序进行初级算术运算。这个就是Dijkstra的双栈法求值（符号栈和数字栈）。表达式由括号、运算符和操作数组成，我们按照以下的规则从左到右将其送入栈处理：

- 将操作数压入数栈
- 将操作符压入符栈
- 忽略左括号
- 遇到右括号时，弹出一个运算符，弹出所需数量的操作数，并将运算符和操作数的运算结果压入操作数栈

在处理完最后一个右括号时，操作数栈上只有一个值，它就是整个表达式的值。这个算法的原理是：每当算法遇到一个被括号包围并且由一个运算符和两个操作数组成的子表达式时，它都将运算符和操作数的计算结果压入操作数栈。这样的结果就好像在输入中用这个值代替了该子表达式，因此用这个值代替子表达式得到的结果和原来的表达式是相同的。我们可以反复应用这个规律并得到一个最终值。参见`Evaluation.js`。

#### 括号匹配问题

匹配括号是否成对出现。`[()]{}{[()()]()}`为true，而`[(])`为false。与双栈法求值算法类似：遇到左括号(`(`,`[`或者`{`)的时候入栈，遇到右括号的时候出栈判断匹配问题。

#### 括号补全问题

从标准输入得到一个缺少左括号的表达式并打印出补全括号之后的中序表达式。`1 + 2 ) * 3 - 4 ) * 5 - 6 ) ) )` => `((1 + 2) * ((3 - 4) * (5 - 6)))`

- 使用两个栈分别保存数值和操作符，分别为opStack和valueStack。顺序处理输入字符串的字符：
- 如果为操作符，压入opStack。
- 否则，如果为右括号，从valueStack取出两个操作数，从opStack取出1个操作符，添加括号组合后压入valueStack。
- 否则，为数字，压入valueStack。
以上处理办法需要输出满足以下条件，也就是有如下的假设：输入表达式是合法的。

#### 中缀转后缀

与算术表达式求值使用的算法一样，值栈和符号栈。扫描字符
- 忽略左括号
- 遇到数字压入valueStack
- 遇到符号压入opStack
- 遇到右括号从valueStack中弹出2个操作数，从opStack中弹出一个操作符，计算后缀表达式压入valueStack
- 最后valueStack中的值就是后缀表达式

#### 后缀表达式求值

- 思路类似双栈法，但是扫描字符串的时候遇到操作符的时候就需要计算结果了，可以省掉opStack
- 遇到数字压入valueStack
- 遇到操作符从valueStack中弹出2个元素，并进行相关操作后压入valueStack
- 后缀表达式的值就是栈顶的值

#### 环形链表实现的队列

```js
class Queue {
    constructor() {
        this.last = null;
    }
    enqueue(item) {
        const node = new Node(item);
        if (this.last == null) {
            this.last = node;
            node.next = node;
        } else {
            node.next = this.last.next;
            this.last.next = node;
            this.last = node;
        }
    }
    dequeue() {
        if (this.isEmpty()) return null;
        const item = this.last.next.value;
        if (this.last.next == this.last) {
            this.last = null;
        } else {
            this.last.next = this.last.next.next;
        }
        return item;
    }
    isEmpty() {
        return this.last === null;
    }
}
```

### 参考资料

- [《算法》](https://github.com/aistrate/AlgorithmsSedgewick)
- [算法4的数据文件](http://git-hexo-blog.oss-cn-beijing.aliyuncs.com/algs4-data.zip)

## 难点

- 1.3.49
- 2.2.25
