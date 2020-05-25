// 只出现一次的数字
// 给定一个非空整数数组，除了某个元素只出现一次以外，其余每个元素均出现两次。找出那个只出现了一次的元素。
//
// 说明：
//
// 你的算法应该具有线性时间复杂度。 你可以不使用额外空间来实现吗？
//
// 示例 1:
//
// 输入: [2,2,1]
// 输出: 1
// 示例 2:
//
// 输入: [4,1,2,1,2]
// 输出: 4

/**
 * @param {number[]} nums
 * @return {number}
 */
var singleNumber = function (nums) {
  // 统计每个数字出现的次数
  // const counter = new Map();
  // for (const num of nums) {
  //   let count = counter.get(num) || 0;
  //   counter.set(num, ++count);
  // }
  // for (const [k, v] of counter) {
  //   if (v === 1) {
  //     return k;
  //   }
  // }

  // 如果集合中没有元素，则添加进集合，否则从集合中删除
  // 这样如果某个元素的出现次数为2，则第一次加入，第二次 删除，集合中留下的都是出现一次的元素
  // const set = new Set();
  // for (const num of nums) {
  //   if (set.has(num)) {
  //     set.delete(num);
  //   } else {
  //     set.add(num);
  //   }
  // }
  //
  // return [...set][0];

  // function sum(arr) {
  //   return arr.reduce((p, c) => p + c);
  // }
  //
  // // 数学法:2*(a+b+c) - (a  + a + b + b + c)  = c
  // return 2 * sum([...new Set(nums)]) - sum(nums);

  // 位操作：异或法
  // a ^ 0 = a
  // a ^ a = 0
  // a ^ b ^ a = (a ^ a ^ b) = 0 ^ b = b
  let result = 0;
  for (const num of nums) {
    result = result ^ num;
  }
  return result;
};
