// 按照partition方法的轨迹格式给出该方法是如何切分数组EASYQUESTION的

// 说明：# 前面的是标定点元素

// n = 12

// sort(0,11)
// partition(0,11),v = E,E A S Y Q U E S T I O N
// i = 0,j = 12
// 从左向右扫描，找到第一个大于等于v的元素 S, i = 2
// 从右向左扫描，找到第一个小于等于v的元素 E, j = 6
// i < j，交换元素,新数组
// E A E Y Q U S S T I O N
// 从i=2，向右扫描，找到第一个大于等于v的元素Y,i = 3
// 从j=6，向左扫描，招待第一个小于等于v的元素E ,j = 2
// 把v = E，放置到2
// E A E Y Q U S S T I O N

// sort(0,1) , sort(3,11)
// partition(0,1), v = E,E A
// i = 0,j = 2
// 从左向右扫描，找到第一个大于等于v的元素，未果，i = 2
// 从右向左扫描，找到第一个小于等于v的元素 A, j = 1
// i >  j，不交换
// 把v = E放到正确的位置 j
// A E

// sort(3,11)
// partition(3,11) ,v = Y, Y Q U S S T I O N
// i = 3,j = 12
// 从左向右扫描，找到第一个大于等于Y的元素,未果，i = 12
// 从右向左扫描，找到第一个小于等于Y的元素N，j = 11
// i > j，不交换
// 把v = Y放到j
// N Q U S S T I O Y

// sort(3,10),sort(11,11)
// partition(3,10) ,v = N, N Q U S S T I O
// i = 3,j = 11
// ->，找到第一个大于等于N的元素Q,i = 4
// <-，找到第一个小于等于N的元素I,j = 9
// i < j,交换
// N I U S S T Q O
// 从i = 4向右找到第一个大于等于N的元素U,i = 5
// 从j = 9向左找到第一个小于等于N的元素I,j = 4
// 将v = N放到j
// I N U S S T Q O

// sort(3,3),sort(4,10)
// sort(3,3)跳过
// sort(4,10),partition(4,10),v = I, I U S S T Q O
// i = 4,j = 11
// ->，找到第一个大于等于I的元素U,i = 5
// <-，找到第一个小于等于I的元素，未果，j到达左边界4
// 将v = I放到j
// I U S S T Q O

// sort(4,3)跳过,sort(5,10)
// partition(5,10),v = U,U S S T Q O
// i = 5,j = 11
// ->，找到第一个大于等于U的元素，i = 11
// <-，找到第一个小于等于U的元素O，j = 10
// 把v = U，放到正确的位置
// O S S T Q U

// sort(5,9),sort(11,10)跳过
// partition(5,9),v = O,O S S T Q
// ->，找到第一个大于等于O的元素S，i = 6
// <-，找到第一个小于等于O的元素Q,j = 9
// i < j ，交换
// O Q S T S
// 从i = 6向右，找到第一个大于等于O的元素S,i = 7
// 从j = 9向左，找到第一个小于等于O的元素O,到达左边界j = 5
// 将v = O放到j
// O Q S T S

// sort(5,4)跳过,sort(6,9)
// partition(6,9)，v = Q,Q S T S
// ->，找到第一个大于等于Q的元素S，i = 7
// <-，找到第一个小于等于Q的元素Q,到达左边界j = 6
// 将v = Q放到j
// Q S T S

// sort(6,5)跳过，sort(7,9)
// partition(7,9),v = S, S T S
// ->，找到第一个大于等于S的元素T，i = 8
// <-，找到第一个小于等于S的元素S,j = 9
// i < j，交换
// S T S
// 从i=8向右，找到第一个大于等于S的元素T,i = 9
// 从j=9向左，找到第一个大于等于S的元素S，到达左边界j = 7
// 将v = S，放到正确的位置
// S T S

// sort(7,6)跳过，sort(8,9)
// partition(8,9),v = T,T S
// ->，找到第一个大于等于T的元素，未果i = 10
// <-，找到第一个小于等于T的元素S,j = 9
// 将v = T，放到正确位置
// S T

// A E E I N O Q S S T U Y
