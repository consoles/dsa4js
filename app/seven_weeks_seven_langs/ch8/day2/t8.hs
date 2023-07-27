-- 编写一个函数，用来确定两个整数的最大公约数

myGcd :: Integer -> Integer -> Integer
myGcd a b
  | b == 0    = a
  | otherwise = myGcd b (a `mod` b)

main = do
    let result1 = myGcd 12 18
    let result2 = myGcd 48 60
    let result3 = myGcd 17 23
    print result1 -- 6
    print result2 -- 12
    print result3 -- 1
