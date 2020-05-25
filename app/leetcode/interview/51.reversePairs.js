/**
 * @param {number[]} nums
 * @return {number}
 */
var reversePairs = function (nums) {

  // 二重循环暴力枚举 超时
  // let count = 0;
  // for (let i = 0; i < nums.length - 1; i++) {
  //   for (let j = i + 1; j < nums.length; j++) {
  //     if (nums[j] < nums[i]) {
  //       count++;
  //     }
  //   }
  // }
  //
  // return count;

  // 用归并排序的思想
  // const aux = new Array(nums.length);
  //
  // function merge(l, mid, r) {
  //   let c = 0;
  //   for (let k = l; k <= r; k++) {
  //     aux[k] = nums[k];
  //   }
  //   let i = l, j = mid + 1;
  //   for (let k = l; k <= r; k++) {
  //     if (i > mid) {
  //       // i已经走过中间了，说明左半部分的区间都是少于右边的
  //       nums[k] = aux[j++];
  //     } else if (j > r) {
  //       // j已经到达了右边界，说明有半部分的逆序对的区间已经在if的第3个分支计算过了
  //       nums[k] = aux[i++];
  //     } else if (aux[j] < aux[i]) {
  //       // 特别注意相等的时候不计算逆序对
  //       // arr[j] < arr[i] 可以推断出[i,mid]都大于arr[j]
  //       // 这个时候的数组可以看成 左边[l,i-1],[i,mid] 右边[mid+1,j] [j+1,r]，
  //       // 后半部分比前面部分小，则存在逆序,整个数组中比nums[j]大的区间为[i,mid]和[j+1,r]，但是[j+1,r]是在j后面的，所以是顺序对，一共(mid  - i + 1)个逆序对
  //       nums[k] = aux[j++];
  //       c += (mid - i + 1);
  //     } else {
  //       nums[k] = aux[i++];
  //     }
  //   }
  //   return c;
  // }
  //
  // function count(l, r) {
  //   let c = 0;
  //   if (l >= r) return c;
  //   const mid = l + parseInt((r - l) / 2);
  //   c += count(l, mid);
  //   c += count(mid + 1, r);
  //   c += merge(l, mid, r);
  //   return c;
  // }
  //
  // return count(0, nums.length - 1);

  // 还有一种解法是树状数组，等有时间再看
};
