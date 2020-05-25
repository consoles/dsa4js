/**
 * @param {number} n - a positive integer
 * @return {number} - a positive integer
 */
var reverseBits = function (n) {
  // // arr是颠倒后的二进制，将这个数组转化为十进制即可
  // const arr = [];
  // while (n) {
  //   arr.push(n % 2);
  //   n = parseInt(n / 2);
  // }
  // while (arr.length < 32) {
  //   arr.push(0);
  // }
  // let res = 0;
  // // let pow = 31;
  // for (const num of arr) {
  //   // res += (num * 2 ** (pow--));
  //   res = res * 2 + num;
  // }
  // return res;

  // 居然超时，WTF
  // 从右向左遍历输入整数的位字符串(n = n >> 1)，要检查整数最右边的位，用与计算 n & 1
  // 对于每个位，我们将其反转到正确的位置即 (n & 1) << power，然后添加到最终结果
  // n == 0 的时候停止迭代
  // let ret = 0, power = 31;
  // while (n !== 0) {
  //   ret += (n & 1) << power;
  //   n = n >> 1;
  //   power--;
  // }
  // return ret;

  // function reverseByte(byte, cache) {
  //   if (cache.has(byte)) {
  //     return cache.get(byte);
  //   }
  //   const v = (byte * 0x0202020202 & 0x010884422010) % 1023;
  //   cache.set(byte, v);
  //   return v;
  // }
  //
  // // 使用记忆化
  // const cache = new Map();
  // let ret = 0, power = 24;
  // while (n) {
  //   ret += reverseByte(n & 0xff, cache) << power;
  //   n = n >> 8;
  //   power -= 8;
  // }
  // return ret;

  n = ((n & 0xffff0000) >>> 16) | ((n & 0x0000ffff) << 16);
  n = ((n & 0xff00ff00) >>> 8) | ((n & 0x00ff00ff) << 8);
  n = ((n & 0xf0f0f0f0) >>> 4) | ((n & 0x0f0f0f0f) << 4);
  n = ((n & 0xcccccccc) >>> 2) | ((n & 0x33333333) << 2);
  n = ((n & 0xaaaaaaaa) >>> 1) | ((n & 0x55555555) << 1);
  return n >>> 0;
};

console.log(reverseBits(43261596));
