// 问题规模。设在你的计算机上用 TwoSumFast、TwoSum、ThreeSumFast 以及 ThreeSum 能够处理的问题规模为 2^P × 10^3 个整数。使用 DoublingRatio 估计 P 的最大值。

// 把运行时限设置为1s

// twosum当输入倍增的时候，运行时间变成了2 ^ 2 = 4倍，处理8K数据耗时40ms，则1s内能处理21K = 8K * Math.log2(1000 / 40 / 4)
// twosumfast当输入翻倍的时候，运行时间变成了原来2倍，处理8K数据耗时5ms，则1s能处理53K
// threesum当输入翻倍的时候，运行时间变成原先的2 ^ 3=8倍，1s内只能处理1 - 2K
// threesumfast当输入翻倍的时候，运行时间变成原来的2 ^ 2 = 4倍，1s只能处理4K