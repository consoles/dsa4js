// 给定一个整数数组 nums 和一个目标值 target，请你在该数组中找出和为目标值的那 两个 整数，并返回他们的数组下标。
//
// 你可以假设每种输入只会对应一个答案。但是，你不能重复利用这个数组中同样的元素。
//
// 示例:
//
//   给定 nums = [2, 7, 11, 15], target = 9
//
// 因为 nums[0] + nums[1] = 2 + 7 = 9
// 所以返回 [0, 1]
//
// 来源：力扣（LeetCode）
// 链接：https://leetcode-cn.com/problems/two-sum
//   著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。

/**
 * @param {number[]} nums
 * @param {number} target
 * @return {number[]}
 */
var twoSum = function (nums, target) {

  // 暴力枚举N^2
  // const n = nums.length;
  // for (let i = 0; i < n; i++) {
  //   for (let j = i + 1; j < n; j++) {
  //     if (nums[i] + nums[j] === target) {
  //       return [i, j];
  //     }
  //   }
  // }

  // 两遍哈希表2N
  // const map = new Map();
  // for (let i = 0; i < nums.length; i++) {
  //   map.set(nums[i], i);
  // }
  // for (let i = 0; i < nums.length; i++) {
  //   const item = target - nums[i];
  //   if (map.has(item) && map.get(item) !== i) {
  //     return [i, map.get(item)];
  //   }
  // }

  // 一遍hash表 N
  // 在进行迭代并将元素插入到表中的同时，我们还会回过头来检查表中是否已经存在当前元素所对应的目标元素。如果它存在，那我们已经找到了对应解，并立即将其返回。
  const map = new Map();
  for (let i = 0; i < nums.length; i++) {
    const item = target - nums[i];
    if (map.has(item)) {
      return [map.get(item), i];
    }
    map.set(nums[i], i);
  }
};

nums = [2, 7, 11, 15], target = 9;
let ret = twoSum(nums, target);
console.log(ret);
