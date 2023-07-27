-- 编写函数sort，接受一个列表作为参数并且返回一个有序的列表。

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []  -- 如果列表为空，则返回空列表
insertionSort (x:xs) = insert x (insertionSort xs)  -- 将第一个元素插入已排序的剩余部分
  where
    insert x [] = [x]  -- 如果已排序的列表为空，则将元素作为结果列表的唯一元素
    insert x (y:ys)
      | x <= y = x : y : ys  -- 如果要插入的元素小于等于当前元素，则将元素插入当前位置
      | otherwise = y : insert x ys  -- 否则，将元素插入剩余部分的合适位置

main = do
    -- [1,2,3,4,5]
    print (insertionSort [2,3,4,1,5])
