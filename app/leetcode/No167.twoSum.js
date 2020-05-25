/**
 * @param {number[]} numbers
 * @param {number} target
 * @return {number[]}
 */
var twoSum = function (numbers, target) {
  const res = [];
  if (!numbers) return res;
  const len = numbers.length;
  let i = 0, j = len - 1;
  while (i < j) {
    const sum = numbers[i] + numbers[j];
    if (sum === target) {
      // 题目要求下标居然不是从0开始的，真是奇葩
      return [i + 1, j + 1];
      // 去重
      while (i < j && numbers[i] === numbers[i + 1]) i++;
      while (i < j && numbers[j] === numbers[j - 1]) j--;
      i++;
      j--;
    } else if (sum < target) {
      i++;
    } else {
      j--;
    }
  }
};

twoSum([2, 7, 11, 15], 9);
