// 32位计算机中的内存需求。
// 给出 32 位计算机中 Integer、Date、Counter、int[]、double[]、double[][]、String、Node 和 Stack（链表表示）对象所需的内存， 设引用需要 4 字节，表示对象的开销为 8 字节，所需内存均会被填充为 4 字节的倍数。

// 对象+填充 = 12，所有对象共有

// Integer:4(int) + 12
// Date: 3*4(int*3) + 12
// Counter:4(String引用) + 12
// int[]:4(数组长度) + 12 + 4*N
// double[]:4(数组长度) + 12 + 8*N
// double[][]:4(数组长度) + 12 + 4M(引用) + M * (16+8N)(M个一位数组)
// String:4 * 3(int * 3) + 4(字符数组引用) + 12
// Node:4*2(引用*2) + 12
// Stack:4(引用) + 4(int) + 12