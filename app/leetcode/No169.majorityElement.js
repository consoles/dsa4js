/**
 * @param {number[]} nums
 * @return {number}
 */
var majorityElement = function (nums) {
  // 计数法
  // const limit = Math.floor(nums.length / 2);
  // const counter = new Map();
  // for (const num of nums) {
  //   let count = counter.get(num) || 0;
  //   if (++count > limit) {
  //     return num;
  //   }
  //   counter.set(num, count);
  // }
  // return -1;

  // 排序思路：数组中有出现次数 > n / 2的元素，那么排好序之后相同元素总是相邻的，即存在长度 > n / 2的一长串相同元素构成的连续子数组
  // nums.sort((a, b) => a - b);
  // return nums[nums.length >> 1];

  // 元素出现的次数大于n/2,可以理解为求n/2+1小的问题，典型的求第k大(小)的问题,可以借助快排的思想
  // function quickSearch(nums, l, r, k) {
  //   const j = partition(nums, l, r);
  //   if (j === k) {
  //     return nums[j];
  //   }
  //   return j > k ? quickSearch(nums, l, j - 1, k) : quickSearch(nums, j + 1, r, k);
  // }
  //
  // function partition(nums, l, r) {
  //   const v = nums[l];
  //   let i = l, j = r + 1;
  //   while (true) {
  //     while (i < r && nums[++i] > v) ;
  //     while (j > l && nums[--j] < v) ;
  //     if (i >= j) {
  //       break;
  //     }
  //     [nums[i], nums[j]] = [nums[j], nums[i]];
  //   }
  //   [nums[l], nums[j]] = [nums[j], nums[l]];
  //   return j;
  // }
  //
  // return quickSearch(nums, 0, nums.length - 1, parseInt(nums.length / 2));

  // 摩尔投票法（这就相当于每个“多数元素”和其他元素 两两相互抵消，抵消到最后肯定还剩余至少1个“多数元素”。）
  // 候选人candNum初始化为nums[0]，票数初始化为1
  // 当遇到和candNum相同的数，则票数count++，否则count--，当count为0的时候更换候选人，并将count重置为1
  // 遍历完数组后candNum即为最终答案
  let candNum = nums[0], count = 1;
  for (let i = 1; i < nums.length; i++) {
    const num = nums[i];
    if (num === candNum) {
      ++count;
    } else {
      if (--count === 0) {
        candNum = num;
        count = 1;
      }
    }
  }
  return candNum;
};
