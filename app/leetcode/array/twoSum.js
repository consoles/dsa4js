// 两数之和

// 给定一个整数数组 nums 和一个目标值 target，请你在该数组中找出和为目标值的那 两个 整数，并返回他们的数组下标。
//
// 你可以假设每种输入只会对应一个答案。但是，你不能重复利用这个数组中同样的元素。
//
// 示例:
//
//   给定 nums = [2, 7, 11, 15], target = 9
//
// 因为 nums[0] + nums[1] = 2 + 7 = 9
// 所以返回 [0, 1]

/**
 * @param {number[]} nums
 * @param {number} target
 * @return {number[]}
 */
var twoSum = function (nums, target) {
  // 方法1，二重循环
  // for (let i = 0; i < nums.length; i++) {
  //   for (let j = i + 1; j < nums.length; j++) {
  //     if (target === nums[i] + nums[j]) {
  //       return [i, j];
  //     }
  //   }
  // }

  // 边遍历边查找
  // const map = new Map();
  // for (let i = 0; i < nums.length; i++) {
  //   const num = nums[i];
  //   const item = target - num;
  //   if (map.has(item)) {
  //     return [i, map.get(item)];
  //   }
  //   map.set(num, i);
  // }

  // 第一遍：记录数字到 索引的映射，第二遍在映射中查找（方法2的退化版本）
  const map = new Map();
  nums.forEach(((value, index) => {
    map.set(value, index);
  }));
  // 扫描，然后查找
  for (let i = 0; i < nums.length; i++) {
    const item = target - nums[i];
    if (map.get(item) !== i) {
      return [i, map.get(item)];
    }
  }
};

nums = [3, 2, 4], target = 6;
const ret = twoSum(nums, target);
debugger;
