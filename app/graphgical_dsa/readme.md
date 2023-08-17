[图解数据结构与算法](https://m.douban.com/book/subject/33377417)

本来想用 golang 来实现，奈何 `go run` 生成的临时 exe 文件会被深信服的 EDR 拦截，删除。所以换用脚本语言中的 python 来实现

冒泡排序实际复杂度为 O(n^2)，选择排序复杂度为 O(n^2 / 2)，选择排序比冒泡排序快 1 倍。

冒泡排序，选择排序，插入排序的复杂度都是 O(N^2)，但是冒泡稳定是 O(N^2)，选择稳定为 O(N^2 / 2)，插入排序的复杂度最低是 O(N)，最高是 O(N^2)，平均为 O(N^2 / 2)。因此，数据几乎有序的情况下，插入排序更优；几乎逆序的情况下，选择排序更优，平均情况下插入和选择的性能是差不多的（考虑到程序的局部性原理，可能实际上插入排序更优）。

快速排序如果不采用随机选择标定点，在数组完全有序或者完全逆序的情况下，复杂度为O(N^2)。

## 无序数组中选择第 k 大的元素

首先想到的是对数组进行排序，然后选择第 k 大的元素，这需要 O(N * lg N) 的复杂度，这也不算差，但是一种名为 *快速选择* 的算法可以做得更好。快速选择需要对数组进行分区，这和快排类似（或者可以将它想象成快排和二分查找的结合）。

分区的作用就是把轴放到正确的格子上，快排就是利用了这一点。

[quick_select-1](https://git-hexo-blog.oss-cn-beijing.aliyuncs.com/graphgical_dsa_quick_select.png)
[quick_select-2](https://git-hexo-blog.oss-cn-beijing.aliyuncs.com/graphgical_dsa_quick_select2.png)

```python
# 完整代码参见：sortable_array.py
# 快排的思想找出数组中的第 k 小的值 
def quickselect(self, kth_lowest_value, l, r)    :
    # 基准场景：数组只剩一个元素，就找到我们需要的值了
    if r - l <= 0:
        return self.array[l]
    
    p = self.partition(l, r)
    if p == kth_lowest_value:
        return self.array[p]
    if kth_lowest_value < p:
        return self.quickselect(kth_lowest_value, l, p - 1)
    else:
        return self.quickselect(kth_lowest_value, p + 1, r)
```

# 链表

双向链表能高效实现队列这种底层数据结构。在头部尾部插入和删除的时间复杂度都是 O(1)。因此用双向链表实现队列这种数据结构再合适不过了。

# 二分搜索树

理想情况下，查找一个元素的复杂度为 O(lgN)，和有序数组的二分查找复杂度相同，但是 BST 的插入和删除也是 O(lgN)，这一点比有序数组的 O(N) 强

# 迪杰斯特拉算法

![使用迪杰斯特拉算法求最便宜的机票](https://git-hexo-blog.oss-cn-beijing.aliyuncs.com/dijkstra_min_plane_price.png)

1. 以起点的顶点为当前顶点
2. 检查当前顶点的所有邻接点，计算起点到所有已知顶点的权重，并记录下来
3. 从**未访问过** (未曾作为当前顶点)的邻接点中，选取一个起点能到达的总权重*最小*的顶点作为当前顶点
4. 重复 1-3, 直到图中所有的顶点都被访问过

![便宜机票1](https://git-hexo-blog.oss-cn-beijing.aliyuncs.com/dijkstra_min_plane_price1.png)
![便宜机票2](https://git-hexo-blog.oss-cn-beijing.aliyuncs.com/dijkstra_min_plane_price2.png)
![便宜机票3](https://git-hexo-blog.oss-cn-beijing.aliyuncs.com/dijkstra_min_plane_price3.png)

没怎么看懂，需要进一步思考。
