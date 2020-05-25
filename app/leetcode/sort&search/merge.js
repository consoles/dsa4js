/**
 * @param {number[]} nums1
 * @param {number} m
 * @param {number[]} nums2
 * @param {number} n
 * @return {void} Do not return anything, modify nums1 in-place instead.
 */
var merge = function (nums1, m, nums2, n) {
  // 方法1，合并到后半段，然后对nums1整体排序
  // for (let i =  0;i < n;i++) {
  //   nums1[m++] = nums2[i];
  // }
  // nums1.sort((a,b) => a-b);
  // return m;

  // 方法2，引入辅助数组，然后归并
  // let i = 0, j = 0, arr = [];
  // while (i < m || j < n) {
  //   let item = -1;
  //   if (i === m) {
  //     item = nums2[j++];
  //   } else if (j === n) {
  //     item = nums1[i++];
  //   } else if (nums1[i] <= nums2[j]) {
  //     item = nums1[i++];
  //   } else {
  //     item = nums2[j++];
  //   }
  //   arr.push(item);
  // }
  // // 复制
  // m = arr.length;
  // for (let i = 0; i < m; i++) {
  //   nums1[i] = arr[i];
  // }
  // return m;

  // 方法3，nums1复制一份出来
  // const nums1Copy = nums1.slice();
  // let i = 0, j = 0, k = 0;
  // while (i < m || j < n) {
  //   let item = -1;
  //   if (i === m) {
  //     item = nums2[j++];
  //   } else if (j === n) {
  //     item = nums1Copy[i++];
  //   } else if (nums1Copy[i] <= nums2[j]) {
  //     item = nums1Copy[i++];
  //   } else {
  //     item = nums2[j++];
  //   }
  //   nums1[k++] = item;
  // }
  // return k;

  // 方法4，从后向前归并
  let i = m - 1, j = n - 1, k = m + n - 1;
  while (i >= 0 || j >= 0) {
    if (i < 0) {
      nums1[k] = nums2[j--];
    } else if (j < 0) {
      nums1[k] = nums1[i--];
    } else if (nums1[i] >= nums2[j]) {
      nums1[k] = nums1[i--];
    } else {
      nums1[k] = nums2[j--];
    }
    k--;
  }

  return m + n - 1;
};

nums1 = [1, 2, 3, 0, 0, 0], m = 3;
nums2 = [2, 5, 6], n = 3;

merge(nums1, m, nums2, n);
debugger;
