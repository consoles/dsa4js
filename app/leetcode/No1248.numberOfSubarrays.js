/**
 * @param {number[]} nums
 * @param {number} k
 * @return {number}
 */
var numberOfSubarrays = function (nums, k) {
  // const n = nums.length;
  // // odd数组用来记录第i个奇数的下标,[odd[i]~odd[i+k]-1]这个子数组就包含k个奇数的子数组，有这个区间向两端发散，所以存在一个更大的区间（两端是偶数的）包含这个区间，也满足有k个 奇数
  // // 由于我们已经记录了每个奇数的下标，所以我们知道对于第i个奇数，它的前一个奇数的下标为odd[i-1]，也就是说odd[i-1]~odd[i]之间的数都是偶数，同理可得odd[i+k-1],odd[i+k]之间的数也是偶数
  // // 因此我们可以得出包含k个奇数的子数组的左区间为(odd[i-1],odd[i]]，右区间为[odd[i+k-1],odd[i+k])，左边和右边的组合一共有 (odd[i] - odd[i-1]) * (odd[i+k] - odd[i+k-1]) 个
  // const odd = new Array(n + 2);
  // // cnt表示奇数的个数
  // let ans = 0, cnt = 0;
  // for (let i = 0; i < n; i++) {
  //   // 奇数
  //   if (nums[i] & 1) {
  //     odd[++cnt] = i;
  //   }
  // }
  // odd[0] = -1;
  // odd[++cnt] = n;
  // for (let i = 1; i + k <= cnt; i++) {
  //   ans += (odd[i] - odd[i - 1]) * (odd[i + k] - odd[i + k - 1]);
  // }
  // return ans;

  // 前缀和  +  哈希表
  // let sum = 0;
  // let res = 0;
  // const map = new Map();
  // map.set(0, 1); // 前缀和为0，出现的次数 为1
  // for (const num of nums) {
  //   sum += (num & 1);
  //   // 当前前缀和是sum，尝试在map中查找 是否存在键值是sum-k(即前缀和是sum-k) ，若找到，即找到子序列和是k
  //   if (map.has(sum - k)) {
  //     res += map.get(sum - k);
  //   }
  //   map.set(sum, (map.get(sum) || 0) + 1);
  // }
  // return res;

  // https://leetcode-cn.com/problems/count-number-of-nice-subarrays/solution/count-number-of-nice-subarrays-by-ikaruga/359670/
  // 滑动窗口
  // let res = 0;
  // const odd = [-1]; // 记录奇数的索引，并在头尾插入两条假数据
  // // i为窗口开始，j为窗口结束，保证窗口内有k+1个奇数，即odd.size -  i > k
  // // 当滑动一个窗口的时候计算刚脱离窗口的前面的个数left =  odd[i] - odd[i-1]，计算窗口后面的个数right = j - odd[odd.size  - 2]
  // // 从i到odd[odd.size-2]含有k个奇数的"优美子数组"组合为left*right，累加 结果即可
  // let i = 1;
  // for (let j = 0; j <= nums.length; j++) {
  //   if (j === nums.length || nums[j] & 1) {
  //     odd.push(j);
  //   }
  //   if (odd.length - i > k) {
  //     const left = odd[i] - odd[i - 1];
  //     const right = j - odd[odd.length - 2];
  //     res += left * right;
  //     i++;
  //   }
  // }
  // return res;

  let left = 0, right = 0, oddCnt = 0, res = 0;
  while (right < nums.length) {
    // 右指针先走，每遇到一个奇数，则oddCnt++
    if ((nums[right++] & 1) === 1) {
      oddCnt++;
    }
    // 若当前滑动窗口[left,right)中有k个奇数，进入此分支统计当前滑动窗口中的优美子数组个数
    if (oddCnt === k) {
      // 先将滑动窗口的右边界向右拓展 ，直到遇到下一个奇数（或者出界）
      // rightEvenCnt即为第k个奇数右边的偶数的个数
      const tmp = right;
      while (right < nums.length && (nums[right] & 1) === 0) {
        right++;
      }
      const rightEvenCnt = right - tmp;
      // leftEvenCnt即为第1个 奇数左边 的偶数的个数
      let leftEvenCnt = 0;
      while ((nums[left] & 1) === 0) {
        leftEvenCnt++;
        left++;
      }
      // 之所以加1是因为我们可以一个偶数都不选
      res += (leftEvenCnt + 1) * (rightEvenCnt + 1);
      // 此时，left指向的是第一个奇数，因为该区间已经统计完了，因此left向右移动oddCnt--;
      left++;
      oddCnt--;
    }
  }
  return res;
};

nums = [1, 1, 2, 1, 1], k = 3;

numberOfSubarrays(nums, k);
