// 两个数组的交集 II

// 给定两个数组，编写一个函数来计算它们的交集。
//
// 示例 1:
//
// 输入: nums1 = [1,2,2,1], nums2 = [2,2]
// 输出: [2,2]
// 示例 2:
//
// 输入: nums1 = [4,9,5], nums2 = [9,4,9,8,4]
// 输出: [4,9]
// 说明：
//
// 输出结果中每个元素出现的次数，应与元素在两个数组中出现的次数一致。
// 我们可以不考虑输出结果的顺序。
// 进阶:
//
//   如果给定的数组已经排好序呢？你将如何优化你的算法？
// 如果 nums1 的大小比 nums2 小很多，哪种方法更优？
// 如果 nums2 的元素存储在磁盘上，磁盘内存是有限的，并且你不能一次加载所有的元素到内存中，你该怎么办？

/**
 * @param {number[]} nums1
 * @param {number[]} nums2
 * @return {number[]}
 */
var intersect = function (nums1, nums2) {
  // let smallArr = nums1;
  // let bigArr = nums2;
  // if (nums1.length > nums2.length) {
  //   smallArr = nums2;
  //   bigArr = nums1;
  // }
  // //
  // // const res = [];
  // // // 扫描小数组中的每一个元素，查找大数组中是否有对应元素
  // // for (const num of smallArr) {
  // //   let index = bigArr.indexOf(num);
  // //   if (index !== -1) {
  // //     bigArr.splice(index, 1);
  // //     res.push(num);
  // //   }
  // // }
  // //
  // // return res;
  //
  // //一个 计数器 计算数组中每个元素出现的次数
  // const counter = new Map();
  // for (const num of bigArr) {
  //   let count = counter.get(num) || 0;
  //   counter.set(num, ++count);
  // }
  //
  // const res = [];
  // for (const num of smallArr) {
  //   const count = counter.get(num);
  //   if (count > 0) {
  //     res.push(num);
  //     counter.set(num, count - 1);
  //   }
  // }
  // return res;

  nums1.sort((a, b) => a - b);
  nums2.sort((a, b) => a - b);

  // 双指针，i指向nums1,j指向nums2，移动较小的指针直到两者指向的数字相同
  let i = 0; // 指向nums1
  let j = 0; // 指向nums2
  let k = 0;

  while (i < nums1.length && j < nums2.length) {
    if (nums1[i] < nums2[j]) {
      i++;
    } else if (nums2[j] < nums1[i]) {
      j++;
    } else {
      nums1[k++] = nums1[i];
      i++;
      j++;
    }
  }

  nums1.length = k;
  return nums1;
};
