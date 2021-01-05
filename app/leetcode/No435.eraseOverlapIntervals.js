/**
 * @param {number[][]} intervals
 * @return {number}
 */
var eraseOverlapIntervals = function (intervals) {
    if (intervals.length <= 1) return 0;
    // 按照区间的起点进行排序
    intervals.sort((a, b) => a[0] - b[0]);
    // 记录区间尾部的位置
    let end = intervals[0][1];
    // 需要移除的数量
    let count = 0;
    for (let i = 1; i < intervals.length; i++) {
        const interval = intervals[i];
        if (interval[0] < end) {
            // 区间重叠了，必须要移除一个，移除尾部比较大的，保留尾部比较小的。
            // 当前区间[3,6]，后面区间是[4,5]，则保留[4,5]，移除[3,6]
            // 当前区间[3,6]，后面区间是[5,9]，则保留[3,6]，移除[4.5]
            // 我们贪心的认为现有区间应该越短越好，这样后面的区间就越不容易形成重叠区间
            end = Math.min(end, interval[1]);
            count++;
        } else {
            // 没有区间重叠，就不需要移除，只需要更新尾部的位置即可
            end = interval[1];
        }
    }
    return count;
};


