// 2.1.15
// 昂贵的交换。
// 一家货运公司的一位职工得到了一项任务，需要将若干大货箱按照发货时间摆放。
// 比较发货时间很容易（对照标签即可），但将两个货箱交换位置则很困难（移动麻烦）。
// 仓库已经快满了，只有一个空闲的仓位。这位职员应该使用哪种排序算法呢？

// 选择排序

// 交换（也就是 Exch() 方法）需要一个额外空间，这里的条件满足。
// 现在我们应该使交换次数最少，选择排序只需要 N 次交换，比插入排序平均 N ^ 2 / 4 少（N > 2）。