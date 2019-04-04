// 画出下面的 id[] 数组所对应的树。这可能是加权 quick-union 算法得到的结果吗？解释为什么不可能，或者给出能够得到该数组的一系列操作。

//                  i                             0 1 2 3 4 5 6 7 8 9
// ------------------------------------------------------------------------------------------------
//                 id[i]                          1 1 3 1 5 6 1 3 4 5

//       1
//    /  |  \
//   0   3   6
//      / \  |
//     7   2 5
//          / \    
//         4   9 
//         |
//         8

// 这颗树的结构如上图：由于加权quick-union树的最大深度为lg(10) < 4，因此不可能是通过加权quick-union得到