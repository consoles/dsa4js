from collections import defaultdict

class Solution:
    def countLargestGroup(self, n: int) -> int:
        counter = defaultdict(list[int])
        for i in range(1, n + 1):
            s = 0
            while i != 0:
                s += i % 10
                i //= 10
            counter[s].append(i)
        max_len = 0
        for v in counter.values():
            max_len = max(max_len, len(v))
        res = 0
        for v in counter.values():
            if len(v) == max_len:
                res += 1
        return res

    def countLargestGroup2(self, n: int) -> int:
        counter = defaultdict(list[int])
        # 在迭代的同时，更新最大长度，可以少一次循环
        max_len = 0
        for i in range(1, n + 1):
            s = 0
            while i != 0:
                s += i % 10
                i //= 10
            counter[s].append(i)
            if len(counter[s]) > max_len:
                max_len = len(counter[s])
        res = 0
        for v in counter.values():
            if len(v) == max_len:
                res += 1
        return res

    def countLargestGroup3(self, n: int) -> int:
        # 我们其实并不关心 counter 的值（当前数字本身）
        counter = defaultdict(int)
        res = max_len = 0
        for i in range(1, n + 1):
            s = 0
            while i != 0:
                s += i % 10
                i //= 10
            counter[s] += 1
            if counter[s] > max_len:
                max_len = counter[s]
                res = 1
            elif counter[s] == max_len:
                res += 1
        return res

# 4 
# n = 13

# 2
# n = 2

# 6
# n = 15

# 5
n = 24
r = Solution().countLargestGroup(n)
print(r)
