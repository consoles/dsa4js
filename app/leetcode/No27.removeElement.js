/**
 * @param {number[]} nums
 * @param {number} val
 * @return {number}
 */
var removeElement = function (nums, val) {
  // let i = 0;
  // for (let j = 0; j < nums.length; j++) {
  //   if (nums[j] !== val) {
  //     nums[i++] = nums[j];
  //   }
  // }
  // return i;

  // 方法2：双指针，适用于当要删除的元素很少时，将当前等于val的元素和最后一个位置交换
  let i = 0, j = nums.length - 1;
  while (i < j) {
    // 请注意，被交换的最后一个元素可能是您想要移除的值。但是不要担心，在下一次迭代中，我们仍然会检查这个元素。
    if (nums[i] === val) {
      nums[i] = nums[j - 1];
      j--;
    } else {
      i++;
    }
  }
  return j;
};
