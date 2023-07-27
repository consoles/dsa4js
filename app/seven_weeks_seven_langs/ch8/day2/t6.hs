--  编写一个函数，该函数接受一个参数x，并返回从x起始，每两个元素间相隔差值为2的惰性序列。然后，编写另外一个函数，返回从y开始，每两个元素间相隔差值为4的惰性序列。通过组合将两个函数合并在一起返回一个从x+y开始，每两个元素间相隔差值为7的惰性序列。

lazySequence2 :: Integer -> [Integer]
lazySequence2 x = [x, x + 2 ..]

lazySequence4 :: Integer -> [Integer]
lazySequence4 x = [x, x + 4 ..]

combinedSequence :: Integer -> Integer -> [Integer]
combinedSequence x y = zipWith (+) (lazySequence2 x) (lazySequence4 y)

main = do
    -- [0,2,4,6,8,10,12,14,16,18]
    print (take 10 (lazySequence2 0))
    -- [1,3,5,7,9,11,13,15,17,19]
    print (take 10 (lazySequence2 1))
    -- [0,4,8,12,16,20,24,28,32,36]
    print (take 10 (lazySequence4 0))
    -- [1,5,9,13,17,21,25,29,33,37]
    print (take 10 (lazySequence4 1))
    -- [3,9,15,21,27]
    print(take 5 (combinedSequence 1 2))
    -- [15,21,27]
    print(take 3 (combinedSequence 10 5))
