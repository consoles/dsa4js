--  runhaskell .\gamma.hs
main = do
    print "hello world"
    print (4 + 1)
    print (2 + 3.0 * 5)
    print ("I love" ++ " you")
    print ('a')
    -- 等价于字符串 "ab"，字符串其实是字符列表
    print (['a', 'b'])

    print "Boolean type:"
    -- False
    print ((5 + 5) /= 10)
