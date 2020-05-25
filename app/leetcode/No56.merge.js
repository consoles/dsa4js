/**
 * @param {number[][]} intervals
 * @return {number[][]}
 */
var merge = function (intervals) {
  // if (!intervals || intervals.length <= 1) return intervals;
  // // 按照区间的起始点升序排列
  // intervals.sort((a, b) => a[0] - b[0]);
  //
  // // 每个区间都是合法区间，即start <= end
  // const res = [];
  // for (const interval of intervals) {
  //   // 该区间的左端点大于已合并区间的右端点
  //   if (res.length === 0 || interval[0] > res[res.length - 1][1]) {
  //     res.push(interval);
  //   } else {
  //     res[res.length - 1][1] = Math.max(res[res.length - 1][1], interval[1]);
  //   }
  // }
  // return res;

  // 排序 + 双指针
  if (!intervals || intervals.length <= 1) return intervals;
  intervals.sort((a, b) => {
    const diff = a[0] - b[0];
    return diff !== 0 ? diff : a[1] - b[1];
  });

  // 左指针i指向当前区间的开始，变量t记录连续的范围，右指针向后搜寻，如果后续区间的开始值比t小，说明区间可以合并，更新更大的范围t
  // 当区间断开的时候，将t作为区间的结束，存储到答案里，移动左指针，跳过中间已经合并的区间
  let res = [];
  let i = 0;
  while (i < intervals.length) {
    let t = intervals[i][1];
    let j = i + 1;
    while (j < intervals.length && intervals[j][0] <= t) {
      t = Math.max(t, intervals[j][1]);
      j++;
    }
    res.push([intervals[i][0], t]);
    i = j;
  }
  return res;
};

a = [[1, 3], [2, 6], [8, 10], [15, 18]];
// a = [[1, 4], [4, 5]];
// a = [[1, 4], [2, 3]];
merge(a);
