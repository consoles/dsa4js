--  编写一个函数，该函数使用Maybe monad查找一个散列表值。编写一个散列表，该散列表可以多级存储其他的散列表。使用Maybe monad检索一个散列key对应的多级散列表中的元素。

lookup_value key [] = Nothing
lookup_value key ((candidate, value):rest)
    | candidate == key = Just value
    | otherwise = lookup key rest

test_data = [("something", [("message", "Hello, world!")]),
            ("empty", [])]

lookup_message hash = lookup_value "message" hash

get_message = lookup_value "something" test_data
                >>= lookup_message
                >>= return

get_nothing = lookup_value "empty" test_data
                >>= lookup_message
                >>= return

main = do
    -- Just "Hello, world!"
    print get_message
    -- Nothing
    print get_nothing
