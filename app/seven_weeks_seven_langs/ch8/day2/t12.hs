--  对上面的练习，将函数加到左侧和右侧，使得每行文字用空格补齐（使得两个页边笔直）。

splitLinesWithMargin :: Int -> Int -> String -> [(Int, String)]
splitLinesWithMargin width margin = zip [1..] . splitLines width
  where
    splitLines :: Int -> String -> [String]
    splitLines width = map addMargin . go . words
      where
        go [] = []
        go (w:ws) = case breakLine w ws of
          (line, rest) -> line : go rest

        breakLine w [] = (w, [])
        breakLine w (x:xs)
          | length w + 1 + length x <= width = breakLine (w ++ " " ++ x) xs
          | otherwise = (w, x:xs)

        addMargin line = replicate margin ' ' ++ line ++ replicate (width - length line) ' '

printListComprehension :: Show a => [a] -> IO ()
printListComprehension xs = mapM_ print xs

main :: IO ()
main = do
    let result = splitLinesWithMargin 10 1 "This is a long string that needs to be split into multiple lines"
    -- [(1," This is a "),(2," long      "),(3," string    "),(4," that needs"),(5," to be     "),(6," split into"),(7," multiple  "),(8," lines     ")]
    print result
    print "----------------"
    printListComprehension result
