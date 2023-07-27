-- 为上一个练习添加行号

splitLinesWithLineNumber :: Int -> String -> [(Int, String)]
splitLinesWithLineNumber width = zip [1..] . splitLines width
  where
    splitLines :: Int -> String -> [String]
    splitLines width = go . words
      where
        go [] = []
        go (w:ws) = case breakLine w ws of
          (line, rest) -> line : go rest

        breakLine w [] = (w, [])
        breakLine w (x:xs)
          | length w + 1 + length x <= width = breakLine (w ++ " " ++ x) xs
          | otherwise = (w, x:xs)

printListComprehension :: Show a => [a] -> IO ()
printListComprehension xs = mapM_ print xs

main = do
    let result = splitLinesWithLineNumber 10 "This is a long string that needs to be split into multiple lines"
    -- [(1,"This is a"),(2,"long"),(3,"string"),(4,"that needs"),(5,"to be"),(6,"split into"),(7,"multiple"),(8,"lines")]
    print result
    print "----------------"
    printListComprehension result
