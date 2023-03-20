// 使用递归和循环 2 种方法实现斐波那契数列

fib := method(n,
    if (n <= 2, 1, fib(n - 1) + fib(n - 2))
)

fib(6) println // 8

fib2 := method(n,
    if (n <= 2, 1)
    f1 := 1
    f2 := 1
    result := 0
    for(i, 3, n,
        result = f1 + f2
        f1 = f2
        f2 = result
    )
    result
)

fib2(6) println // 8
