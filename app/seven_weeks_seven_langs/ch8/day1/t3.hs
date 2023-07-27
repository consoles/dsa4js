-- 你能找到多少种实现allEven的方法？

-- 方法一：使用模式匹配和递归
-- 这个实现使用了递归和模式匹配来遍历列表，检查每个元素是否为偶数。当列表为空时，返回 True。否则，提取列表的头部元素 x 和尾部元素 xs，检查 x 是否为偶数，然后递归地检查剩余的元素。
allEven1 :: [Integer] -> Bool
allEven1 [] = True
allEven1 (x:xs) = (x `mod` 2 == 0) && allEven1 xs

-- 方法二：使用 filter 和 length 函数
-- 这个实现使用了 filter 和 length 函数。filter even xs 返回一个列表，其中包含 xs 中所有偶数元素，然后使用 length 函数计算这个列表的长度。如果这个长度等于 xs 的长度，那么 xs 中的所有元素都是偶数，返回 True；否则返回 False。
allEven2 :: [Integer] -> Bool
allEven2 xs = (length (filter even xs)) == length xs

-- 方法三：使用 foldr 函数
-- 这个实现使用了 foldr 函数。foldr 函数对列表进行折叠，从右到左依次处理列表中的每个元素。使用一个 lambda 函数作为 foldr 的第一个参数，这个 lambda 函数接受两个参数 x 和 acc，其中 x 是当前处理的元素，acc 是上一次处理的结果。如果 x 是偶数，返回 (even x) && acc，否则返回 False。最终的结果就是 foldr 函数的返回值。
allEven3 :: [Integer] -> Bool
allEven3 xs = foldr (\x acc -> (even x) && acc) True xs

-- 方法四：使用 all 函数
allEven4 :: [Integer] -> Bool
allEven4 xs = all even xs

main = do
    -- false
    print (allEven1 [1, 2, 3, 4, 5])
    print (allEven2 [1, 2, 3, 4, 5])
    print (allEven3 [1, 2, 3, 4, 5])
    print (allEven4 [1, 2, 3, 4, 5])

-- true
    print (allEven1 [0, 2, 4, 6, 8])
    print (allEven2 [0, 2, 4, 6, 8])
    print (allEven3 [0, 2, 4, 6, 8])
    print (allEven4 [0, 2, 4, 6, 8])
