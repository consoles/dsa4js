# 快排

快排是一种原地排序（只需要一个很小的辅助栈）,且将长度为 N 的数组排序所需要的时间和 N*lgN 成正比。快排的内循环比大多数排序算法都要短小。但是也是非常脆弱的，需要非常小心才能避免低劣的性能。

快排和归并时互补的。归并排序将数组分成 2 个子数组分别排序，并将有序的子数组归并以将整个数组排序；快排将的胡须排序的方式则是当两个子数组都有序的时候，整个子数组就自然有序了。在归并排序中，递归调用发生在处理整个数组之前；在快排中递归调用发生在处理整个数组之后。在归并中，一个数组被2等分；在快排中，切分(partition)的位置取决于数组的内容。

快排的关键在于partition，这个过程使得数组满足下面的3个条件：

- 对于某个j,arr[j]已经排定
- arr[lo]到arr[j-1]中所有元素都不大于arr[j]
- arr[j+1]到arr[hi]中所有元素都不小于arr[j]

经过partition后arr[j]到了正确的位置

实现切分策略是：

![快排中的切分](https://blog-cdn-bmnleumcou.cn-shenzhen.fcapp.run?a=images/quick-sort-partition.png)

随机选择arr[lo]作为切分元素，即那个将会被排定的元素，然后从数组左端开始向右扫描直到找到一个大于等于标定点的元素，再从数组右端开始向左扫描直到找到一个小于等于标定点的元素。这两个元素显然是没有排定的，因此我们可以交换它们的位置。如此继续，我们就可以保证左指针i左侧的元素都不大于标定点，右指针j的右侧元素都不大于标定点。当两个指针相遇的时候我们只需要标定点arr[lo]和左数组最右侧的元素arr[j]交换然后返回j即可。

```javascript
/**
 * 将数组切分为arr[lo...i-1],arr[i],arr[i+1...hi]
 */
function _partition(arr, lo, hi) {

  let i = lo,j = hi + 1; // 左右扫描指针
  const v = arr[lo]; // 切分元素

  while (true) {
    // 扫描左右，检查扫描是否结束并交换元素
    while (arr[++i] < v) {
      if (i === hi) {
        break;
      }
    }
    while (v < arr[--j]) {
      if (j === lo) {
        break;
      }
    }
    if (i >= j) {
      break;
    }
    swap(arr, i, j);  // 在切分点右侧找到一个小于标定点的元素，在切分点左侧找到一个大于标定点的元素，交换位置
  }
  swap(arr, lo, j); // 将v = arr[j]放到正确的位置
  // arr[lo...j-1] <= arr[j] <= arr[j+1...hi]达成
  return j;
}

// 这种partition由一个指针从左向右扫描，对于完全相同的元素，复杂度退化为O(N^2)，解决方案是上面的双路快排
function _partition2(arr, lo, hi) {
  const v = arr[lo];
  // arr[l+1...j] < v;arr[j+1...i) > v
  let j = lo;
  // 从左向右扫描
  for (let i = lo + 1; i <= hi; i++) {
    // 如果当前元素小于标定点，则将当前元素和大于v的第一个元素交换，当前元素融入了小于v的部分
    if (arr[i] < v) {
      swap(arr, ++j, i);
    }
    // 如果当前元素大于v则融入到大于v的部分i自增就行了
  }
  swap(arr, lo, j);
  return j;
}

function _quickSort(arr, lo, hi) {
  if (hi <= lo) return;
  const j = _partition(arr, lo, hi);
  _quickSort(arr, lo, j - 1);
  _quickSort(arr, j + 1, hi);
}

const quickSort = arr => {
  arr = shuffle(arr); // 消除对输入的依赖
  _quickSort(arr, 0, arr.length - 1);
};
```

如果去掉排序初始时候打乱数组的操作，则对于一个完全顺序的数组，快排将会到达最大的复杂度O(N^2)

上述partition中按照arr[lo]的值v进行切分。当指针i和j相遇的时候主循环退出，在循环中，arr[i]小于v的时候增大i，arr[j]大于v的时候减小j，然后交换arr[i]和arr[j]来保证i左侧的元素都不大于v，j右侧的元素都不小于v。当指针相遇的时候交换arr[lo]和arr[j]，切分结束（这样切分值就留在arr[j]中了）。

![快排的切分轨迹](https://blog-cdn-bmnleumcou.cn-shenzhen.fcapp.run?a=images/partion-step.png)

左侧扫描最好是在遇到大于等于标定点的时候停下，右侧扫描则是在遇到小于等于标定点的元素时停下，这样可能会将一些不必要的*等值*元素交换（因此是不稳定的），但是这样可以保证在大量重复元素的时候算法退化为平方级别。

切分方法的内循环只做比较操作，因而性能高于归并和希尔排序。快排的另一个优势是比较次数较少。

如果不采用打乱数组，那么每次选择标定点的时候都是选的当前最小的，比较次数为N*(N-1) / 2。

## 算法改进

- 小数组使用插入排序：5-15的任意值
- 三取样切分：使用子数组的一小部分元素的中位数来切分数组
- 熵最优的排序：大量重复元素的情况下可以将线性代数级别提高到线性级别：三路快排（将数组切分为小于、等于、大于三部分）。参考:[荷兰国旗问题](https://blog.csdn.net/SunnyYoona/article/details/43488481)

```javascript
// 三取样切分
function partition(arr, lo, hi) {
  let i = lo, j = hi + 1;
  if (arr[lo + 1] < arr[lo]) {
    swap(arr, lo + 1, lo);
  }
  if (arr[lo + 2] < arr[lo]) {
    swap(arr, lo + 2, lo);
  }
  // lo最小,放在最前面了，接下来比较arr[lo+1]&arr[lo+2]
  if (arr[lo + 2] < arr[lo + 1]) {
    swap(arr, lo + 2, lo + 1);
  }
  // arr[lo],arr[lo+1],arr[lo+2]从小到大排列了
  swap(arr, lo, lo + 1); // 中位数放左侧
  swap(arr, hi, lo + 2); // 较大的值放在最右侧作为哨兵
  const v = arr[lo];
  while (true) {
    while (arr[++i] < v) ;
    while (arr[--j] > v) ;
    if (i >= j) break;
    swap(arr, i, j);
  }
  swap(arr, lo, j);
  return j;
}

function _quickSort(arr, lo, hi) {
  if (hi <= lo) return;
  // 两个数组的元素直接排序，因为partition寻找中位数的过程中数组长度至少为3
  if (hi === lo + 1) {
    if (arr[hi] < arr[lo]) {
      swap(arr,lo,hi);
    }
    return;
  }
  const j = partition(arr, lo, hi);
  _quickSort(arr, lo, j - 1);
  _quickSort(arr, j + 1, hi);
}

function quickSort(arr) {
  shuffle(arr);
  _quickSort(arr, 0, arr.length - 1);
}
```

对于任意输入都只需要少于7次比较的5取样算法,同理，可以实现五取样切分：

```javascript
function quickSort5Partition(arr) {
  shuffle(arr);
  _sort(arr, 0, arr.length - 1);
  assert(isSorted(arr));
}

function _partition(arr, lo, hi) {
  // a:arr[lo],
  // b:arr[lo+1],
  // c:arr[lo+2],
  // d:arr[lo+3],
  // e:arr[lo+4]

  // 首先对 b c 排序
  if (arr[lo + 1] > arr[lo + 2]) {
    swap(arr, lo + 1, lo + 2);
  }
  // 然后再排序 d e
  if (arr[lo + 3] > arr[lo + 4]) {
    swap(arr, lo + 3, lo + 4);
  }
  // 这时满足 b < c, d < e,比较 b d，把较小的一组放到 b c 的位置上去
  if (arr[lo + 3] < arr[lo + 1]) {
    swap(arr, lo + 1, lo + 3);
    swap(arr, lo + 2, lo + 4);
  }
  // 这时满足 b < c, b < d < e, 交换 a 和 b
  swap(arr, lo, lo + 1);
  // 重新排序 b c
  if (arr[lo + 2] < arr[lo + 1]) {
    swap(arr, lo + 2, lo + 1);
  }
  // 这时再次满足 b < c, d < e,比较 b d，把最小的一组放到 b c 的位置上去
  if (arr[lo + 3] < arr[lo + 1]) {
    swap(arr, lo + 1, lo + 3);
    swap(arr, lo + 2, lo + 4);
  }
  // 这时 a 和 b 为五个数中的最小值和次小值（顺序不固定，a 也可以是次小值）,d e为5个数中的最大值和次大只
  // 最后比较 c 和 d，较小的那一个即为中位数（即第三小的数）
  if (arr[lo + 3] < arr[lo + 2]) {
    swap(arr, lo + 3, lo + 2);
  }
  swap(arr, lo + 2, lo);
  // d e 放到数组末尾充当哨兵
  swap(arr, lo + 3, hi);
  swap(arr, lo + 4, hi - 1);

  // 调整指针位置，前2位和2两位已经在何时的位置了
  let i = lo, j = hi + 1;
  i += 2;
  j -= 2;
  const v = arr[lo];
  while (true) {
    while (arr[++i] < v) ;
    while (arr[--j] > v) ;
    if (i >= j) break;
    swap(arr, i, j);
  }
  swap(arr, lo, j);
  return j;
}

function _sort(arr, lo, hi) {
  if (hi <= lo) return;
  const len = hi - lo + 1;
  // 少于5个元素直接进行插入排序
  if (len < 5) {
    for (let i = lo; i <= hi; i++) {
      for (let j = i; j > 0 && arr[j] < arr[j - 1]; j--) {
        swap(arr, j, j - 1);
      }
    }
    return;
  }
  const j = _partition(arr, lo, hi);
  _sort(arr, lo, j - 1);
  _sort(arr, j + 1, hi);
}
```

## 快排的其他实现

### 随机选择标定点

上述实现的快排在排序之前会将数组打乱，这是非常消耗性能的，我们可以在partition的过程中随机一个索引并和lo交换，这是非常快的操作（虽然每次partition都会进行）

```javascript
function partition(arr,lo,hi) {
  const index = _.random(lo, hi);
  swap(arr, index, lo);

  const v = arr[lo];
  let i = l, j = r + 1;
  while (true) {
    while (this.arr[++i] < v) if (i === r) break;
    while (this.arr[--j] > v) if (j === l) break;
    if (i >= j) break;
    swap(arr, i, j);
  }
  swap(arr, j, l);
  return j;
}
```

### 基于栈的快排

```javascript
// partition过程同快排
function quickSortStack(arr) {
  shuffle(arr);
  const s = new Stack();
  // 用一个栈保存切分子数组的左右端点
  s.push([0, arr.length - 1]);
  while (!s.isEmpty()) {
    const [lo, hi] = s.pop();
    if (lo >= hi) continue;
    const j = partition(arr, lo, hi);
    const item1 = [lo, j - 1];
    const item2 = [j + 1, hi];
    // 较大的子数组入栈
    if (j - lo > hi - j) {
      s.push(item1);
      s.push(item2);
    } else {
      s.push(item2);
      s.push(item1);
    }
  }
  assert(isSorted(arr));
}
```

由于自己实现的栈比函数调用的栈性能低，所以操作会慢一点。

### 三路快排

```javascript
function _quickSort3Ways(arr, lo, hi) {
  if (hi <= lo) return;
  let lt = lo, i = lo + 1, gt = hi;
  const v = arr[lo];
  while (i <= gt) {
    if (arr[i] < v) {
      swap(arr, lt++, i++);
    } else if (arr[i] > v) {
      swap(arr, i, gt--);
    } else {
      i++;
    }
  }
  // 现在arr[lo...lt-1] < v = arr[lt...gt] < arr[gt+1...hi]
  _quickSort3Ways(arr, lo, lt - 1);
  _quickSort3Ways(arr, gt + 1, hi);
}

const quickSort3Ways = arr => {
  arr = shuffle(arr);
  _quickSort3Ways(arr, 0, arr.length - 1);
};
```

从左到右遍历数组一次，维护三个指针lt，i，gt。[lo,lt-1]中的元素小于v，[gt+1,hi]中的元素大于v，[lt,i-1]中的元素等于v。[i,gt]中是还没有被扫描过的元素：

- arr[i] < v,交换arr[lt]和arr[i]，lt和i都自增
- arr[i] > v，交换arr[gt]和arr[i]，gt自减
- arr[i] == v，将i加1

以上操作为减少区间[i,gt]从而跳出循环，除非和切分元素相等，其他元素都会被交换。上面的代码能将和切分元素相等的元素归位，这样它们就不会被包含在递归调用的子数组中了

![三向切分](https://blog-cdn-bmnleumcou.cn-shenzhen.fcapp.run?a=images/pattition3ways.png)

这种算法不流行的原因是：比标准的二分法多了很多次交换，但是对于大量重复元素的数组，复杂度从线性代数级别降低到线性级别。这种对重复元素的适应性使得3向快排称为库函数的最佳选择————将大量重复元素进行排序的用例很常见。

### 快速三向切分

用将重复元素放置于数组两端的方式实现一个信息量最优的排序算法。使用两个索引p和q，使得arr[lo...p-1]和arr[q+1...hi]的元素都和arr[lo]相等。使用另外2个索引i和j，使得arr[p...i-1]小于arr[lo],arr[j+1...q]大于arr[lo]。在内循环中加入代码，在arr[i]和v相当时将其与arr[p]交换（并将p加1），在arr[j]和v相等且arr[i]和arr[j]尚未和v进行比较之前将其和arr[q]交换。添加在切分循环后将将和v相等的元素交换到正确位置的代码，这里额外的交换用于和切分元素相等的元素，而正文中的代码将额外的交换用于和切分元素不等的元素

```javascript
// https://www.cnblogs.com/ikesnowy/p/9406468.html
function insertSort(arr, lo, hi) {
  for (let i = lo; i <= hi; i++) {
    for (let j = i; j > 0 && arr[j] < arr[j - 1]; j--) {
      swap(arr, j, j - 1);
    }
  }
}

const INSERTION_SORT_CUTOFF = 8; // 小于这个值采用插入排序
const MEDIAN_OF_3_CUTOFF = 40; // 小于这个值用数组中位数作为枢轴

/**
 * 求三个元素的中位数
 */
function median3(arr, i, j, k) {
  return arr[i] < arr[j] ?
    (arr[j] < arr[k] ? j : arr[i] < arr[k] ? k : i) :
    (arr[k] < arr[j] ? j : arr[k] < arr[i] ? k : i);
}

function _quickSort(arr, lo, hi) {
  if (hi <= lo) return;
  const n = hi - lo + 1;
  if (n <= INSERTION_SORT_CUTOFF) {
    insertSort(arr, lo, hi);
    return;
  }
  if (n <= MEDIAN_OF_3_CUTOFF) {
    const m = median3(arr, lo, lo + parseInt(n / 2), hi); // 对于较小的数组，直接选择左中右三个元素中的中位数作为枢轴
    swap(arr, m, lo);
  } else {
    const eps = parseInt(n / 8);
    const mid = lo + parseInt(n / 2);
    // 选择3组，每组3个元素，分别取3组元素的中位数，然后3个中位数的中位数作为切分元素
    const m1 = median3(arr, lo, lo + eps, lo + 2 * eps);
    const m2 = median3(arr, mid - eps, mid, mid + eps);
    const m3 = median3(arr, hi - 2 * eps, hi - eps, hi);
    const ninther = median3(arr, m1, m2, m3); // 对于较大的数组使用 Turkey Ninther 作为枢轴。
    swap(arr, ninther, lo);
  }
  // 三向切分
  let i = lo, j = hi + 1;
  let p = lo, q = hi + 1;
  const v = arr[lo];
  while (true) {
    while (arr[++i] < v) if (i === hi) break;
    while (arr[--j] > v) if (j === lo) break;
    if (i === j && arr[i] === v) swap(arr, ++p, i);
    if (i >= j) break;
    swap(arr, i, j);
    if (arr[i] === v) swap(arr, ++p, i);
    if (arr[j] === v) swap(arr, --q, j);
  }
  i = j + 1;
  for (let k = lo; k <= p; k++) {
    swap(arr, k, j--);
  }
  for (let k = hi; k >= q; k--) {
    swap(arr, k, i++);
  }
  _quickSort(arr, lo, j);
  _quickSort(arr, i, hi);
}

function quickSort3Ways(arr) {
  _quickSort(arr, 0, arr.length - 1);
  assert(isSorted(arr));
}
```

jdk的排序库函数就采用了这种方式

### 快排中的哨兵

去掉内循环while中的边界检查。由于切分元素本身就是一个哨兵v不可能小于arr[lo],左侧边界的检查是多余的。要去掉另一个检查，可以在打乱数组后将数组最大元素放在arr[length-1]中，该元素永远不会移动（除非和相等的元素交换），可以在所有包含它的子数组中成为哨兵。

```javascript
function partition(arr, lo, hi) {
  let i = lo, j = hi + 1;
  const v = arr[lo];
  while (true) {
    while (arr[++i] < v) {
      // if (i === hi) {
      //   break;
      // }
    }
    while (arr[--j] > v) {
      // if (j === lo) {
      //   break;
      // }
    }
    if (i >= j) {
      break;
    }
    swap(arr, i, j);
  }
  swap(arr, j, lo);
  return j;
}

function _quickSort(arr, lo, hi) {
  if (hi <= lo) return;
  const j = partition(arr, lo, hi);
  _quickSort(arr, lo, j - 1);
  _quickSort(arr, j + 1, hi);
}

function quickSort(arr) {
  shuffle(arr);
  // 最大元素放到最后一位
  let maxIndex = 0;
  for (let i = 1; i < arr.length; i++) {
    if (arr[i] < arr[maxIndex]) {
      maxIndex = i;
    }
  }
  if (maxIndex !== 0) {
    swap(arr, maxIndex, arr.length - 1);
  }
  _quickSort(arr, 0, arr.length - 1);
}
```

### 使用快排的思想对只有2个元素的数组进行排序

```javascript
// https://algs4.cs.princeton.edu/23quicksort/Sort2distinct.java.html
function sort2Elements(arr) {
  let lt = 0,
    gt = arr.length-1;
  let i = 0;
  while (i <= gt) {
    if (arr[i] < arr[lt]) {
      swap(arr, lt++, i++);
    } else if (arr[i] > arr[lt]) {
      swap(arr, i, gt--);
    } else {
      i++;
    }
  }
}
```

复杂度可以降低到O(N)级别,3个指针lt,i,gt将数组划分为3个区域:[0,lt)小元素,[gt,n-1]大元素，[i,gt)还没有被扫描的元素。

### 螺丝和螺帽

假设有n个螺丝和n个螺帽混在一对，你需要快速将它们配对。一个螺丝只会匹配一个螺帽，一个螺帽也只会匹配一个螺丝(螺丝和螺帽都是不重复的)。你可以尝试着把一个螺丝和一个螺帽拧在一起看看谁大了，但是不能直接比较2个螺丝或者2个螺帽。给出一个解决问题的有效方法

```javascript
const assert = require('assert');
const _ = require('lodash');
const swap = require('../../swap');

function partition(bolts, nuts, lo, hi) {
  let i = lo, j = hi + 1;
  const vB = bolts[lo]; // 螺母的标定点
  // 一:找到和vB螺母配对的螺丝,并交换，此时nuts[lo]和vB就对应了
  for (let k = lo; k <= hi; k++) {
    if (nuts[k] === vB) {
      swap(nuts, k, lo);
      break;
    }
  }
  // 二:使用螺母作为标定点，对螺丝进行排序
  while (true) {
    while (nuts[++i] < vB) {
      if (i === hi) {
        break;
      }
    }
    while (nuts[--j] > vB) {
      if (j === lo) {
        break;
      }
    }
    if (i >= j) {
      break;
    }
    swap(nuts, i, j);
  }
  swap(nuts, lo, j);
  // 用螺丝去比较螺母（经过上述的操作nuts[j]位置的螺母放到了正确的位置）
  // assert(vN === vB); // 这里的vN其实是等于vB的，因为开始的vB = bots[lo],步骤一过后:vB = bots[lo] = nuts[lo],步骤2后nuts[lo]被放到了j位置
  const vN = nuts[j];
  i = lo;
  j = hi + 1;
  while (true) {
    while (bolts[++i] < vN) {
      if (i === hi) {
        break;
      }
    }
    while (bolts[--j] > vN) {
      if (j === lo) {
        break;
      }
    }
    if (i >= j) {
      break;
    }
    swap(bolts, i, j);
  }
  swap(bolts, lo, j);
  return j;
}

function _sortBoltsAndButs(bolts, nuts, lo, hi) {
  if (hi <= lo) return;
  const j = partition(bolts, nuts, lo, hi);
  _sortBoltsAndButs(bolts, nuts, lo, j - 1);
  _sortBoltsAndButs(bolts, nuts, j + 1, hi);
}

function sortBoltsAndButs(bolts, nuts) {
  assert.deepEqual(bolts.length, nuts.length, '螺丝和螺母不配套');
  _sortBoltsAndButs(bolts, nuts, 0, bolts.length - 1);
}

const indexes = _.range(0, 9);

const bolts = _.shuffle(indexes); // 螺母数组
const nuts = _.shuffle(indexes); // 螺母数组

sortBoltsAndButs(bolts,nuts);
```

### 快排的最佳情况

生成quick sort方法表现最佳的数组（无重复元素）：数组大小为N并且不包含重复元素，每次切分后两个子数组的大小最多差1（子数组的大小和含有N个相同元素的数组的切分情况相同）

```javascript
const assert = require('assert');
const _ = require('lodash');
const swap = require('../../swap');

// 中点两边是最佳情况，整个数组就是最佳情况了
// 首先构造一个有序数组，然后找到中点（作为枢轴），对中点左右两侧子数组进行构造，将选择的枢轴放到开始处(a[lo])。
function _quickBest(arr, lo, hi) {
  for (let k = lo; k <= hi; k++) {
    assert.deepStrictEqual(k, arr[k]);
  }
  if (hi <= lo) {
    return;
  }
  const mid = lo + parseInt((hi - lo) / 2);
  _quickBest(arr, lo, mid - 1);
  _quickBest(arr, mid + 1, hi);
  swap(arr, lo, mid);
}

function quickBest(n) {
  const arr = _.range(n);
  _quickBest(arr, 0, n - 1);
  return arr;
}

let ret = quickBest(3); // 1,0,2
ret = quickBest(5); // 2,1,0,3,4
ret = quickBest(6); // 2,1,0,4,3,5
```

### 对数组求select

将数组重新排列，使得arr[k]正好是第k小的元素，k从0开始。具体思路类似二分查找，先切分，如果切分位置小于k，那么有半部分继续切分，否则左半部分继续切分，直到切分位置正好等于k，直接返回arr[k]。注意：整个过程*不需要对数组进行全部排序*。

```js
class QuickPedantic {
  sort(arr) {
    shuffle(arr);
    this._sort(arr, 0, arr.length - 1);
  }

  _sort(arr, lo, hi) {
    if (lo >= hi) return;
    const j = this._partition(arr, lo, hi);
    this._sort(arr, lo, j - 1);
    this._sort(arr, j + 1, hi);
  }

  _partition(arr, lo, hi) {
    const v = arr[lo];
    let i = lo, j = hi + 1;
    while (true) {
      while (arr[++i] < v) {
        if (i === hi) {
          break;
        }
      }
      while (arr[--j] > v) {
        if (j === lo) {
          break;
        }
      }
      if (i >= j) {
        break;
      }
      swap(arr, i, j);
    }
    swap(arr, j, lo);
    return j;
  }

  select(arr, k) {
    assert(k >= 0 && k < arr.length, 'selected elements out of bounds');
    shuffle(arr);
    let lo = 0, hi = arr.length - 1;
    while (hi > lo) {
      const i = this._partition(arr, lo, hi);
      if (i > k) {
        hi = i - 1;
      } else if (i < k) {
        lo = i + 1;
      } else {
        return arr[i];
      }
    }
    return arr[lo];
  }

  select2(arr, k) {
    assert(k >= 0 && k < arr.length, 'selected elements out of bounds');
    return this._select2(arr, k, 0, arr.length - 1);
  }

  /**
   * 递归版本
   */
  _select2(arr, k, lo, hi) {
    if (lo >= hi) return arr[lo];
    const j = this._partition(arr, lo, hi);
    if (j === k) return arr[j];
    return j > k ? this._select2(arr, k, lo, j - 1) : this._select2(arr, k, j + 1, hi);
  }
}
```
