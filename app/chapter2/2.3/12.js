// 按照代码所示轨迹的格式给出信息量最佳的快排第一次是如何切分数组B A B A B A B A C A D A B R A 的

// n = 15
// sort(0,14)
// partition(0,14),i = 0,j = 15,标定点v = B
// 向右，找到第一个大于等于v的元素B,i = 2
// 向左，找到第一个小于等于v的元素A,j = 14
// i < j，交换i，j
// B A A A B A B A C A D A B R B
// i继续向右，找到第一个大于等于v的元素B,i = 4
// j继续向左，找到第一个小于等于v的元B,j = 12
// i < j，交换i，j
// B A A A B A B A C A D A B R B
// i继续向右，找到第一个大于等于v的元素B,i = 6
// j继续向左，找到第一个小于等于v的元素A,j = 11
// i < j，交换i,j
// B A A A B A A A C A D B B R B
// i继续向右，找到第一个大于等于v的元素C,i = 8
// j继续向左，找到第一个小于等于v的元素A,j = 9
// i < j，交换i,j
// B A A A B A A A A C D B B R B
// i继续向右，找到第一个大于等于v的元素D,i = 10
// j继续向左，找到第一个小于等于v的元素A,j = 8
// 将v = B放到j = 8
// A A A A B A A A B C D B B R B

// sort(0,7),sort(9,14)
// partition(0,7),A A A A B A A A,i = 0,j = 8,标定点v = A
// i向右找到第一个大于等于v的元素A,i = 1
// j向左，找到第一个小于等于v的元素A,j = 7
// i < j，交换i，j
// A A A A B A A A
// i继续向右，找到第一个大于等于v的元素A,i = 2
// j继续向左，找到第一个小于等于v的元素A,j = 6
// i < j，交换i，j
// A A A A B A A A
// i继续向右，找到第一个大于等于v的元素A,i = 3
// j继续向左，找到第一个小于等于v的元素A,j = 5
// i < j，交换i，j
// A A A A B A A A
// i继续向右，找到第一个大于等于v的元素B,i = 4
// j继续向左，找到第一个小于等于v的元素A,j = 3
// i > j，不交换
// 把v = A放到j=3
// A A A A B A A A

// sort(0,2),sort(4,7)
// partition(0,2),A A A,i = 0,j = 3,标定点v = A
// i向右找到第一个大于等于v的元素A,i = 1
// j向左找到第一个小于等于v的元素A,j = 2
// i < j,交换i,j
// A A A
// i向右找到第一个大于等于v的元素A,i = 2
// j向左找到第一个小于等于v的元素A,j = 1
// 把v = A放到1
// A A A

// sort(0,0),sort(2,2)跳过

// sort(4,7)
// partition(4,7),B A A A,i = 4,j = 8,v=B
// i向右找到第一个大于等于v的元素未果，到达右边界i=7
// j向左找到第一个小于等于v的元素A,j = 7
// 把v = B放到7
// A A A B

// sort(0,7)结果 A A A A A A A B
// sort(9,14)结果 B B B C D R

// 结果 A A A A A A A B B B B B C D R

// sort(9,14)
// partition(9,14),C D B B R B,i = 9,j = 15,标定点v = C
// i向右找到第一个大于等于v的元素D，i = 10
// j向左找到第一个小于等于v的元素B,j = 14
// 交换i，j
// C B B B R D
// i向右找到第一个大于等于v的元R，i = 13
// j向左找到第一个小于等于v的元素B,j = 12
// 把v=C放到j
// B B B C R D

// sort(9,11)
// partition(9,11),B B B,i = 9,j = 12,标定点v = B
// i向右找到第一个大于等于v的元素B，i = 10
// j向左找到第一个小于等于v的元素B,j = 11
// 交换i，j
// B B B
// i向右找到第一个大于等于v的元素B，i = 11
// j向左找到第一个小于等于v的元素B,j = 10
// 把v=B放到j = 10
// B B B

// sort(9,9),sort(11,11) 跳过

// sort(13,14)
// partition(13,14),R D,i = 13,j = 15,标定点v = R
// i向右找到第一个大于等于v的元素，未果，到达右边界i=14跳出
// j向左找到第一个小于等于v的元素D,j = 14
// 将v = R放到j=14
// D R

// sort(13,13),sort(15,14) 跳过
