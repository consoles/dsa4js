-- 高阶函数

squareAll list = map square list
    where square x = x * x

main = do
    -- 匿名函数
    -- "Logic."
    print ((\x -> x) "Logic.")
    -- "USA,  captain."
    print ((\x -> x ++ " captain.") "USA, ")

    -- map 和 where
    -- [1,4,9]
    print (map (\x -> x * x) [1, 2, 3])
    -- [1,4,9]
    print (squareAll [1,2,3])
    -- [2,3,4]
    -- `(+1)` 实际上是一个偏函数应用，函数 `+` 接受 2 个参数，但是实际上只提供了一个，最终结果是得到了一个类似 `(x + 1)` 且仅接受一个参数 x 的函数
    print (map (+ 1) [1,2,3])

    -- filter,foldl,foldr
    -- True
    print (odd 5)
    -- [1,3,5]
    print (filter odd [1,2,3,4,5])
    -- 55，类似 clojure 和 scala，也和 js 中的 reduce 类似
    print (foldl (\x carryOver -> carryOver + x) 0 [1 .. 10])
    -- 55
    print ((foldl (+) 0 [1 .. 10]))

-- 柯里化
    -- 12
    let prod x y = x * y
    print (prod 3 4)
    -- 利用偏函数来创建其他函数
    let double = prod 2
    let triple = prod 3
    -- 6
    print (double 3)
    -- 12
    print (triple 4)

-- 惰性求值
    -- myRange(1 1)
    -- 1:myRange(2 1)
    -- 1:2:myRange(3 1)
    -- 1:2:3:myRange(4 1)
    let myRange start step = start:(myRange (start + step) step)
    -- [10,12,14,16,18]
    print (take 5 (myRange 10 2))

    let lazyFib x y = x:(lazyFib y (x + y))
    let fib = lazyFib 1 1
    let fibNth x = head (drop (x - 1) (take (x) fib))
    -- [1,1,2,3,5]
    print(take 5 (lazyFib 1 1))
    -- [1,1,2,3,5]
    print(take 5 fib)
    -- [10946,17711,28657,46368,75025]
    print(take 5 (drop 20 (lazyFib 1 1)))
    -- 2
    print(fibNth 3)
    -- 8
    print(fibNth 6)
    -- [2,3,5,8,13]
    -- 1 1 2 3 5 8
    --   1 1 2 3 5 8
    --   2 3 5 8 13
    print(take 5 (zipWith (+) fib (drop 1 fib)))
    -- [2,4,6,8,10]
    print(take 5 (map (*2) [1 ..]))
    -- [10,10,20,30,50]
    -- `f . g x` 是 f (g x) 的缩写
    print(take 5 (map ((* 2) . (* 5)) fib))
