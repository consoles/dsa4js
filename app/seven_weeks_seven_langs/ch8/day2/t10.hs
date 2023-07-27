-- 根据恰当的字边界将一个长字符串拆分为多行

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

main = do
    let result = splitLines 10 "This is a long string that needs to be split into multiple lines"
    -- ["This is a","long","string","that needs","to be","split into","multiple","lines"]
    print result
