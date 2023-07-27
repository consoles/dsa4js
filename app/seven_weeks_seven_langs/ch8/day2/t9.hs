-- 创建一个惰性的素数序列

primes :: [Integer]
primes = sieve [2..]
  where
    -- `(p:xs)` 这是一个模式匹配，将无限列表分为两部分：首个元素 p 和剩余的列表 xs
    -- ` [x | x <- xs, x `mod` p /= 0]` 是一个列表推导式，它选择从列表 xs 中筛选出不能整除 p 的元素 x。
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

main = do
    -- [2,3,5,7,11,13,17,19,23,29]
    print (take 10 primes)
