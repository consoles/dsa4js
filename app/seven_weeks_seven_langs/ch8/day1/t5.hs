-- 从五个颜色黑、白、蓝、黄和红任选出两个组合在一起，编写一个函数，计算出所有可能的组合。注意，你只能包含(black, blue)和(blue, black)两者中的一个

colorPairs :: [String] -> [(String, String)]
colorPairs colors = [(a, b) | a <- colors, b <- colors, a < b]

main = do
    let colors = ["black", "white", "blue", "yellow", "red"]
    print ([(a, b) | a <- colors, b <- colors, a < b])
    print "------------------------"
    print (colorPairs colors)
