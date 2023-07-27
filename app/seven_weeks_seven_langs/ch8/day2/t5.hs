--  编写一个Haskell函数，将字符串转换为数字。字符串应该以$2,345,678.99形式提供，并且可以包含前导零。

-- 在Haskell中，Just是一个类型构造器（type constructor），它用于将一个值包装在Maybe类型中。Maybe是Haskell标准库中的一个数据类型，用于表示可能存在也可能不存在的值。
-- Maybe类型具有两个可能的值：Just a和Nothing。当我们需要表示一个值存在时，我们可以使用Just构造器将该值包装在Maybe类型中。例如，Just 42表示一个存在的整数值42。
-- 另一方面，当我们需要表示一个值不存在时，我们可以使用Nothing值。它表示一个空值或者缺失的值。
-- 使用Maybe类型和Just构造器可以帮助我们处理可能出现空值的情况，从而避免空指针异常或者未定义行为。
-- 在这个例子中，Just用于将解析后的数字包装在Maybe Double类型中，以表示解析成功的情况。
-- 如果解析失败，我们返回Nothing表示解析失败的情况。

parseNumber :: String -> Maybe Double
parseNumber str = case filter (/= ',') str of
  '$' : numStr -> readNumber numStr
  numStr -> readNumber numStr
  where
    readNumber numStr = case reads numStr of
      [(num, _)] -> Just num
      _ -> Nothing

main = do
    print (parseNumber "$2,345,678.99")
    print (parseNumber "$012,345.67899")
