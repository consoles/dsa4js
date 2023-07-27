-- 函数
double x = x + x

doubleWithType :: Integer -> Integer
doubleWithType x = x + x

-- 递归，以下语句的顺序十分重要
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)
-- 如果想改变模式的排列顺序可以使用哨兵表达式
factWithGuard :: Integer -> Integer
factWithGuard n
    | n > 1 = n * factWithGuard (n - 1)
    | otherwise = 1

main = do
    -- 4
    print (double 2)
    -- 4
    print (doubleWithType 2)
    -- 120
    print (factorial 5)
    -- 120
    print (factWithGuard 5)
