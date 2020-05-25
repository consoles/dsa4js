/**
 * @param {number[]} nums
 * @return {boolean}
 */
var canJump = function (nums) {

  const cache = {};

  /**
   * 从i位置向后跳
   * @param i
   */
  function jump(i) {
    if (cache[i] !== void 0) return cache[i];
    // 跳到最后位置了
    if (i >= nums.length - 1) {
      return cache[i] = true;
    }
    let step = nums[i];
    if (step === 0) {
      return cache[i] = false;
    }
    if (i + step >= nums.length - 1) {
      return cache[i] = true;
    }
    const flag = jump(i + step);
    if (flag) {
      return cache[i] = true;
    }
    // while (step >= 1) {
    //   const flag = jump(i + step);
    //   if (flag) {
    //     return cache[i] = true;
    //   }
    //   step--;
    // }
    return cache[i] = false;
  }

  // return jump(0);

  // 贪心法
  let k = 0;
  for (let i = 0; i < nums.length; i++) {
    // 不能到达这个位置直接break
    if (i > k) {
      return false;
    }
    k = Math.max(k, i + nums[i]);
    if (k >= nums.length - 1) {
      return true;
    }
  }
  return true;
};

nums = [2,5,0,0];

console.time('1');
ret = canJump(nums);
console.timeEnd('1');
