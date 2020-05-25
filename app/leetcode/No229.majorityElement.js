/**
 * 参见No169
 * 摩尔投票法解决的问题是：如何在任意多的候选人当中选出票数超过一半的那个人
 * @param {number[]} nums
 * @return {number[]}
 */
var majorityElement = function (nums) {
  const res = [];
  if (!nums || nums.length === 0) return res;

  // 题目是找出超过1/3的元素，因此最多有2个元素

  // 初始化2个候选人你candidate和它们的计票
  let cand1 = nums[0], count1 = 0;
  let cand2 = nums[0], count2 = 0;

  // 摩尔投票法，分为2个阶段： 配对阶段 + 计数阶段

  // 配对阶段
  for (const num of nums) {
    // 投票
    if (cand1 === num) {
      count1++;
      continue;
    }
    if (cand2 === num) {
      count2++;
      continue;
    }
    // 第一个候选人配对
    if (count1 === 0) {
      cand1 = num;
      count1++;
      continue;
    }
    if (count2 === 0) {
      cand2 = num;
      count2++;
      continue;
    }
    count1--;
    count2--;
  }
  // 计数阶段
  // 找到2个候选人之后需要确定票数是否大于n / 3
  count1 = 0;
  count2 = 0;
  for (const num of nums) {
    if (cand1 === num) count1++;
    // 这里一定要用else if,因为可能出现[0,0,0]这种用例，导致两个cand是一样的，写两个if结果就变为[0,0]了
    else if (cand2 === num) count2++;
  }
  const limit = parseInt(nums.length / 3);
  if (count1 > limit) res.push(cand1);
  if (count2 > limit) res.push(cand2);

  return res;
};
