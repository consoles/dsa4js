/**
 * @param {number[]} nums
 * @return {number[]}
 */
var singleNumbers = function (nums) {
  // const set = new Set();
  // for (const num of nums) {
  //   if (set.has(num)) {
  //     set.delete(num);
  //   } else {
  //     set.add(num);
  //   }
  // }
  // return [...set];

  // 将所有数进行亦或得到的结果就是那两个不同数的结果 1^4^4^6 = 1^6
  let k = 0;
  for (const num of nums) {
    k ^= num;
  }

  // 获得k中最低位的1
  let mask = 1;
  while ((k & mask) === 0) {
    mask <<= 1;
  }

  let a = 0, b = 0;
  for (const num of nums) {
    if ((num & mask) === 0) {
      a ^= num;
    } else {
      b ^= num;
    }
  }

  return [a, b];
};

const nums = [4, 1, 4, 6];
const ret = singleNumbers(nums);
debugger;
