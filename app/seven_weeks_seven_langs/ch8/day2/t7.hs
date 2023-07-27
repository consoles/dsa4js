-- 使用偏应用函数定义两个函数，其中一个将返回一个数值的一半。另外一个函数会将\n附加到任意字符串的末尾。

half :: Double -> Double
half = (/ 2)

appendNewline :: String -> String
-- 我们使用flip函数将字符串拼接操作符++的顺序翻转，然后使用部分应用操作符$将换行符部分应用到字符串上，实现了将换行符附加到字符串末尾的功能。
appendNewline = flip (++) "\n"

main = do
    let result1 = half 10  -- 5.0
    let result2 = appendNewline "Hello"  -- "Hello\n"
    print result1
    print result2
