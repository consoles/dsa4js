/**
 * @param {number[]} nums
 * @return {number[][]}
 */
var threeSum = function (nums) {
  const res = [];
  if (!nums) return res;
  // 排序 => 双指针
  nums.sort((a, b) => a - b);
  const len = nums.length;
  if (len < 3) return res;
  for (let i = 0; i < len - 2; i++) {
    // 加速1：c为非负数，就不能满足a+b+c=0了
    if (nums[i] > 0) {
      return res;
    }
    // 加速2：跳过计算过的数据，同时防止结果重复
    if (i > 0 && nums[i] === nums[i - 1]) {
      continue;
    }
    let l = i + 1;
    let r = len - 1;
    while (l < r) {
      const sum = nums[i] + nums[l] + nums[r];
      if (sum === 0) {
        res.push([nums[i], nums[l], nums[r]]);
        // 加速3：跳过计算过的数据，同时防止结果重复
        while (l < r && nums[l] === nums[l + 1]) l++;
        while (l < r && nums[r] === nums[r - 1]) r--;
        l++;
        r--;
      } else if (sum < 0) {
        // 使得sum变大的唯一途径是左边的数变大，即l向右移动
        l++;
      } else {
        r--;
      }
    }
  }
  return res;
};

const nums = [-1, 0, 1, 2, -1, -4];
const res = threeSum(nums);
console.log(res);

function twoSum(nums, target) {
  const set = new Set();
  for (const num of nums) {
    const item = target - num;
    if (set.has(item)) {
      return [num, item];
    } else {
      set.add(num);
    }
  }
  return null;
}
