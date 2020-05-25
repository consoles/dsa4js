/**
 * @param {number[]} nums
 * @return {number[]}
 */
var sortArray = function (nums) {
  // 插入排序
  // for (let i = 1; i <= nums.length; i++) {
  //   for (let j = i - 1; j > 0 && nums[j] < nums[j-1]; j--) {
  //     [nums[j],nums[j-1]] = [nums[j-1],nums[j]];
  //   }
  // }

  // 选择排序
  // for (let i  = 0;i < nums.length - 1;i++) {
  //   let minIndex  = i;
  //   for (let j = minIndex;j  < nums.length;j++) {
  //     if (nums[j] < nums[minIndex]) {
  //       minIndex = j;
  //     }
  //   }
  //   if (minIndex !== i) {
  //     [nums[minIndex],nums[i]] = [nums[i],nums[minIndex]];
  //   }
  // }

  // // 一次性分配辅助数组
  // const aux = new Array(nums.length);
  //
  // // 归并排序(自顶向下)
  // function _merge(l, mid, r) {
  //   let i = l, j = mid + 1;
  //   for (let k = l; k <= r; k++) {
  //     if (i > mid) {
  //       aux[k] = nums[j++];
  //     } else if (j > r) {
  //       aux[k] = nums[i++];
  //     } else if (nums[i] < nums[j]) {
  //       aux[k] = nums[i++];
  //     } else {
  //       aux[k] = nums[j++];
  //     }
  //   }
  //   for (let k = l; k <= r; k++) {
  //     nums[k] = aux[k];
  //   }
  // }

  //
  // function _mergeSort(l, r) {
  //   if (l >= r) return;
  //   const mid = l + parseInt((r - l) / 2);
  //   _mergeSort(l, mid);
  //   _mergeSort(mid + 1, r);
  //   _merge(l, mid, r);
  // }
  //
  // _mergeSort(0, nums.length - 1);

  // // 归并排序，从底向上
  // for (let sz = 1; sz < nums.length; sz += sz) {
  //   for (let i = 0; i + sz < nums.length; i += 2 * sz) {
  //     const l = i;
  //     const mid = i + sz - 1;
  //     const r = Math.min(nums.length - 1, mid + sz);
  //     _merge(l, mid, r);
  //   }
  // }

  function _swap(i, j) {
    [nums[i], nums[j]] = [nums[j], nums[i]];
  }

  //
  // // 快速排序
  // function _partition(l, r) {
  //   // 找到一个标定点，使得标定点左边的元素都小于v，标定点右边的元素都大于 v
  //   // 小于v的放左边(大于等于v的自然在右边)
  //   const index = l + parseInt(Math.random() * (r - l + 1));
  //   _swap(index,l);
  //   const v = nums[l]; // 标定元素
  //
  //   let j = l;
  //   for (let i = l + 1; i <= r; i++) {
  //     if (nums[i] < v) {
  //       _swap(++j, i);
  //     }
  //   }
  //   // 标定点元素应该放置在j位置
  //   _swap(l, j);
  //   return j;
  // }
  //
  // function _quickSort(l, r) {
  //   if (l >= r) return;
  //   const j = _partition(l, r);
  //   _quickSort(l, j - 1);
  //   _quickSort(j + 1, r);
  // }
  //
  // _quickSort(0, nums.length - 1);

  // // 冒泡排序
  // for (let i = 0; i < nums.length - 1; i++) {
  //   for (let j = i + 1; j < nums.length; j++) {
  //     if (nums[i] > nums[j]) {
  //       _swap(i, j);
  //     }
  //   }
  // }

  // 堆排序（构造一个最大堆，不断将最大元素置换到数组末尾）
  //  1. 自底向上构造堆（从第一个非叶子节点开始调用sink函数调整堆）
  // let n = nums.length;
  //
  // function _sink(k, n) {
  //   // 左孩子2k+1,右孩子2k+2
  //   while (2 * k + 1 < n) {
  //     let j = 2 * k + 1;
  //     // 存在右孩子并且右孩子比较大，则右孩子应该和父节点交换位置称为新的父节点
  //     if (j + 1 < n && nums[j] < nums[j + 1]) {
  //       j++;
  //     }
  //     if (nums[k] < nums[j]) {
  //       _swap(k, j);
  //       k = j;
  //     } else {
  //       break;
  //     }
  //   }
  // }
  //
  // for (let k = parseInt(n / 2); k >= 0; k--) {
  //   _sink(k, n);
  // }
  //
  // // 2.堆构造完毕，接下来将堆顶元素不断置换到尾部
  // while (n > 0) {
  //   _swap(0, --n);
  //   _sink(0, n);
  // }

  // 计数排序
  const max = Math.max.apply(null, nums);
  const min = Math.min.apply(null, nums);

  const counter = new Array(max - min + 1).fill(0);
  for (const num of nums) {
    counter[num - min]++; // 可能有负数所以做个offset
  }

  let idx = 0;
  for (let i = min; i <= max; i++) {
    let cnt = counter[i - min];
    while (cnt--) {
      nums[idx++] = i;
    }
  }

  return nums;
};

const nums = [5, 1, 1, 2, 0, 0];
const ret = sortArray(nums);
console.log(ret);
