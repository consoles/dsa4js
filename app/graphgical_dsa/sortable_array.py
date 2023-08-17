class SortableArray:
    def __init__(self, array):
        self.array = array

    # https://git-hexo-blog.oss-cn-beijing.aliyuncs.com/graphgical_dsa_quicksort_partition.jpg
    def partition(self, l, r):
        # 总是取最右边的作为轴
        pivot_position = r
        pivot = self.array[pivot_position]

        # 将右指针指向轴左边的一格
        r -= 1

        while True:
            while self.array[l] < pivot:
                l += 1
            while self.array[r] > pivot:
                r -= 1
            if l >= r:
                break
            else:
                self.swap(l, r)

        # 左指针的值和轴交换
        self.swap(l, pivot_position)
        # 根据快排的需要返回左指针
        return l

    def swap(self, index1, index2):
        self.array[index1], self.array[index2] = self.array[index2], self.array[index1]

    # 1. 将数组分区，使轴到正确的位置上
    # 2. 对轴左右两边的子数组递归执行1-2步。也就是说两个子数组各自分区，并形成各自的轴以及轴分隔的更小的子数组。然后也对这些子数组分区，以此类推
    # 3. 当分出的子数组长度为 0 或 1 时，即达到基准情形，无须进一步操作
    def quicksort(self, l, r):
        # 基准场景：分出的子数组长度是 0 或者 1
        if r - l <= 0:
            return

        # 将数组分为 2 部分，并返回分隔所用的轴的索引
        p = self.partition(l, r)

        self.quicksort(l, p - 1)
        self.quicksort(p + 1, r)

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

array = [0, 5, 2, 1, 6, 3]
sortable_array = SortableArray(array)
sortable_array.quicksort(0, len(array) - 1)
print(sortable_array.array)

# 测试第 2 小
array = [0, 50, 20, 10, 60, 30]
sortable_array = SortableArray(array)
print(sortable_array.quickselect(1, 0, len(array) - 1))
