/**
 * @param {number[]} nums
 * @param {number} k
 * @return {number}
 */
var subarraySum = function (nums, k) {
  let count = 0;
  const n = nums.length;

  // 暴力法：求区间[i,j]的和，做了个小优化求[i,j]的和重用了[i,j-1]的计算结果，不然复杂度是O(N^3)
  // for (let i = 0; i < n; i++) {
  //   let sum = 0;
  //   for (let j = i; j < n; j++) {
  //     sum += nums[j];
  //     if (sum === k) {
  //       count++;
  //     }
  //   }
  // }
  // return count;

  // 计算前缀和数组
  // const preSum = new Array(n + 1);
  // preSum[0] = 0;
  // for (let i = 0; i < n; i++) {
  //   preSum[i + 1] = preSum[i] + nums[i];
  // }
  // for (let l = 0; l < n; l++) {
  //   for (let r = l; r < n; r++) {
  //     // 区间和[l,r]，注意下标偏移
  //     if (preSum[r + 1] - preSum[l] === k) {
  //       count++;
  //     }
  //   }
  // }
  // return count;

  // 前缀和 + 哈希表优化
  let preSum = 0;
  const map = new Map();
  map.set(0, 1); // 前缀和 => 前缀和出现的次数
  for (let i = 0; i < n; i++) {
    preSum += nums[i];
    // 计算完包括了当前数前缀和之后，我们去查一查在当前数之前，有多少个前缀和等于preSum - k，这个是因为满足preSum - (preSum - k) == k 的区间的个数是我们所关心的
    // 由当前前缀和寻找 [相减 == k]的目标前缀和，目标前缀和是一个数值，这个数值可能出现不止1次 =>
    if (map.has(preSum - k)) {
      count += map.get(preSum - k);
    }
    const c = map.get(preSum) || 0;
    map.set(preSum, c + 1);
  }
  return count;
};

const nums = [1, 1, 1], k = 2;
subarraySum(nums, k);
