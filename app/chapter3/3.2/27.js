// BinarySearchST: key数组，value数组 + 一个表示大小的整数
// SequentialSearchST:链表（节点） + 一个表示大小的整数
// BST:N个节点

// key是String，value为Integer

// 一个整数4字节,一个引用8字节，还有16字节额外开销
// BinarySearchST = 16 + 16 + 16 * N，当涉及到扩容的时候数组可以变为4N，范围:32 + 16N ~ 32 + 64N
// SequentialSearchST：一个节点有3个引用(key,value,next)， = (16 + 8*3) * N = 40N
// BST:每个节点有4个引用(key,value,left,right) + 1个整数count, = (16 + 8 * 4) = 48N
