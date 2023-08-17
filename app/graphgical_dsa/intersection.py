# 求交集, O(N^2)，无论什么情况都是这个复杂度
def intersection(arr1, arr2):
    result = []
    for item1 in arr1:
        for item2 in arr2:
            if item1 == item2:
                result.append(item1)
    return result

# 复杂度介于 O(N) - (N^2), arr2 数组完全一样的情况下，复杂度为 O(N)
def intersection2(arr1, arr2):
    result = []
    for item1 in arr1:
        for item2 in arr2:
            if item1 == item2:
                result.append(item1)
                # 这里可以优化，在 arr2 中找到 item1 之后, 就没有必要再找了
                break
    return result

print(intersection([1, 2, 3, 4], [2, 3, 4, 5]))
print(intersection2([1, 2, 3, 4], [2, 3, 4, 5]))
