-- 元组和列表
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- 以上 fib 的实现并不高效，使用元组可以得到一个更高效的实现
-- 以下函数接受一个 3元组并返回一个 3元组
fibTuple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
fibTuple (x, y, 0) = (x, y, 0)
fibTuple (x, y, index) = fibTuple (y, x + y, index - 1)
-- fibTuple (0, 1 ,4) -> fibTuple (1, 1 ,3) ->  fibTuple (1, 2 ,2) ->  fibTuple (2, 3 ,1) -> fibTuple (3, 5 ,0)
-- 答案在返回结果的第一个元素位置上,我们可以用下面的方式获得答案
fibResult :: (Integer, Integer, Integer) -> Integer
fibResult (x, y, z) = x
-- 向外暴露一个友好的接口
fibPublic :: Integer -> Integer
fibPublic x = fibResult (fibTuple (0, 1, x))

fibNextPair :: (Integer, Integer) -> (Integer, Integer)
fibNextPair (x, y) = (y, x + y)
fibNthPair :: Integer -> (Integer, Integer)
-- 基本情况
fibNthPair 1 = (1, 1)
fibNthPair n = fibNextPair (fibNthPair (n - 1))
fib2 :: Integer -> Integer
fib2 = fst . fibNthPair

size [] = 0
size (h:t) = 1 + size t

prod [] = 1
prod (h:t) = h * prod t

-- 列表中的所有偶数
allEven :: [Integer] -> [Integer]
allEven [] = []
-- 如果 head 是偶数，则将这个 head 与对列表 tail 部分执行 allEven 的结果合并到一起
-- 如果 head 是奇数，则丢弃它，并返回对列表 tail 部分执行 allEven 的结果
allEven (h:t) = if even h then h : allEven t else allEven t

main = do
    -- (3, 5, 0)
    print (fibTuple (0, 1, 4))
    -- 2178309
    print (fibPublic 32)
    -- 很慢，要好几秒
    -- 2178309
    print (fib 32)

-- 获取列表中的第二个元素
-- 这个函数等价于 second lst = head (tail lst)
    let second = head . tail
    -- let second lst = head (tail lst)
    -- 2
    print (second [1, 2])
    -- 4
    print (second [3, 4, 5])
    -- (21,34)
    print (fibNthPair(8))
    -- (34,55)
    print (fibNthPair(9))
    -- (55,89)
    print (fibNthPair(10))
-- 12
    print (size "hello world.")
-- 24
    print (prod [1, 2, 3, 4])
-- zip 用于列表的合并
-- [(1,4),(2,5),(3,6)]
    print (zip [1, 2, 3] [4, 5, 6])
    -- [2,4,6]
    print (allEven [1, 2, 3, 4, 5, 6])
    -- range
    -- [10.0,9.5,9.0,8.5,8.0,7.5,7.0,6.5,6.0,5.5,5.0,4.5,4.0]
    print ([10, 9.5 .. 4])
    -- [0,2,4,6,8]
    print (take 5 [ 0, 2 ..])
    -- 列表推导
    -- [2,4,6,8]
    print ([x * 2 | x <- [1, 2, 3, 4]])
    -- 按照对角线旋转多边形
    -- [(2,1),(3,2),(1,3)]
    print ([(y, x) | (x, y) <- [(1, 2), (2, 3), (3, 1)]])
    -- 计算全部可能的排列
    -- [("a","a"),("a","b"),("a","c"),("b","a"),("b","b"),("b","c"),("c","a"),("c","b"),("c","c")]
    let strings = ["a", "b", "c"]
    print ([(a, b) | a <- strings, b <- strings])
    -- 计算全部组合
    -- [("a","b"),("a","c"),("b","a"),("b","c"),("c","a"),("c","b")]
    print ([(a, b) | a <- strings, b <- strings, a /= b])
    -- 按照顺序剔除重复的组合
    -- [("a","b"),("a","c"),("b","c")]
    print ([(a, b) | a <- strings, b <- strings, a < b])
