-- 编写一个函数，它以一个列表作为参数并返回逆序后的列表。

-- `++` 表示列表的链接操作
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

main = do
    print (reverse [1, 2, 3, 4, 5])
    print (reverseList [1, 2, 3, 4, 5])
