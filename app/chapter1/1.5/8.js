// 用一个反例证明 quick-find 算法中的 union() 方法的以下直观实现是错误的：
// public void union(int p, int q)
// {
//     if (connected(p, q)) return;
//     for (int I = 0; I < id.length; I++)
//     {
//         // 将p的分量重命名为q的分量
//         if (id[i] == id[p]) id[i] = id[q];
//     }
//     count--;
// }

// 当id数组中有多个元素需要修改时后面的元素可能不会被修改
// id = [0,0,0,1,1,1]
// unoin(0,3)，当i = 0时id[0] === id[p],则id[0] = 1
// 当i = 1时，id[1] = 0，但是此时id[p]则变成了1，id数组最终变成了[1,0,0,1,1,1]