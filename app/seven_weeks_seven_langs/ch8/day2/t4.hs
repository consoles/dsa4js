-- 编写函数sort，接受一个列表和一个比较两个参数大小的函数作为参数，然后返回一个有序列表。

sort :: (a -> a -> Ordering) -> [a] -> [a]
sort _ [] = []  -- 如果列表为空，则返回空列表
sort cmp (x:xs) = insert cmp x (sort cmp xs)  -- 将第一个元素插入已排序的剩余部分
  where
    insert _ x [] = [x]  -- 如果已排序的列表为空，则将元素作为结果列表的唯一元素
    insert cmp x (y:ys)
      | cmp x y == GT = y : insert cmp x ys  -- 如果要插入的元素大于当前元素，则将元素插入剩余部分的合适位置
      | otherwise = x : y : ys  -- 否则，将元素插入当前位置

-- 比较函数示例：按照字符串长度进行比较
compareLength :: String -> String -> Ordering
compareLength s1 s2 = compare (length s1) (length s2)

main = do
    let sortedList = sort compareLength ["abc", "de", "fghij", "k"]
    -- ["k", "de", "abc", "fghij"]
    print sortedList
