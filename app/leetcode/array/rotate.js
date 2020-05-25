// 给定一个数组，将数组中的元素向右移动 k 个位置，其中 k 是非负数。
//
// 示例 1:
//
// 输入: [1,2,3,4,5,6,7] 和 k = 3
// 输出: [5,6,7,1,2,3,4]
// 解释:
//   向右旋转 1 步: [7,1,2,3,4,5,6]
// 向右旋转 2 步: [6,7,1,2,3,4,5]
// 向右旋转 3 步: [5,6,7,1,2,3,4]
// 示例 2:
//
// 输入: [-1,-100,3,99] 和 k = 2
// 输出: [3,99,-1,-100]
// 解释:
//   向右旋转 1 步: [99,-1,-100,3]
// 向右旋转 2 步: [3,99,-1,-100]
// 说明:
//
//   尽可能想出更多的解决方案，至少有三种不同的方法可以解决这个问题。
// 要求使用空间复杂度为 O(1) 的 原地 算法。

/**
 * @param {number[]} nums
 * @param {number} k
 * @return {void} Do not return anything, modify nums in-place instead.
 */
var rotate = function (nums, k) {
  // 偷懒的做法
  // while (k--) {
  //   nums.unshift(nums.pop());
  // }

  // 一次一次模拟向后移动
  // while (k--) {
  //   const last = nums[nums.length - 1];
  //   for (let i = nums.length - 1; i > 0; i--) {
  //     nums[i] = nums[i - 1];
  //   }
  //   nums[0] = last;
  // }

  // // 使用额外数组，原来数组下表为i的元素需要放到(i+k) % n的位置
  // const n = nums.length;
  // const aux = new Array(n);
  // for (let i = 0; i < n; i++) {
  //   aux[(i + k) % n] = nums[i];
  // }
  // // aux是正确的数组，再将其赋值给原来的数组
  // for (let i = 0; i < aux.length; i++) {
  //   nums[i] = aux[i];
  // }

  // 环状替换
  // 如果我们直接把每一个数字放到它最后的位置，但这样的后果是遗失原来的元素。因此，我们需要把被替换的数字保存在变量 temptemp 里面。然后，我们将被替换数字（temptemp）放到它正确的位置，并继续这个过程 nn 次， nn 是数组的长度。
  //
  // 作者：LeetCode
  // 链接：https://leetcode-cn.com/problems/rotate-array/solution/xuan-zhuan-shu-zu-by-leetcode/
  // 来源：力扣（LeetCode）
  // 著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。
  // const n = nums.length;
  // k %= n;
  // let count = 0;
  // for (let start = 0; count < n; start++) {
  //   let current = start;
  //   let prev = nums[start];
  //   do {
  //     const next = (current + k) % n;
  //     const temp = nums[next];
  //     nums[next] = prev;
  //     prev = temp;
  //     current = next;
  //     count++;
  //   } while (start !== current);
  // }

  // 三次翻转
  // 这个方法基于这个事实：当我们旋转数组 k 次， k\%nk%n 个尾部元素会被移动到头部，剩下的元素会被向后移动。在这个方法中，我们首先将所有元素反转。然后反转前 k 个元素，再反转后面 n-kn−k 个元素，就能得到想要的结果。

  function _reverse(nums, start, end) {
    while (start < end) {
      [nums[start], nums[end]] = [nums[end], nums[start]];
      start++;
      end--;
    }
  }

  const n = nums.length;
  k %= n;
  _reverse(nums, 0, n - 1);
  _reverse(nums, 0, k - 1);
  _reverse(nums, k, n - 1);
};

let nums = [1, 2, 3, 4, 5, 6, 7];
rotate(nums, 3);
console.log(nums);
