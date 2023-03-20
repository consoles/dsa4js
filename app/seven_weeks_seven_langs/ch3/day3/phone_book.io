OperatorTable addAssignOperator(":", "atPutNumber") // 冒号被转换为 atPutNumber，key:value 就被转换为 atPutNumber("key", value)

// 遇到大括号执行
curlyBrackets := method(
    r := Map clone
    // r "Bob Jack" : "+86 18733423232" => r atPutNumber("Bob Jack", "+86 18733423232")
    // r "Andy Scott" : "+21 1281928192" => r atPutNumber("Andy Scott", "+21 1281928192")
    call message arguments foreach(arg,
        r doMessage(arg)
    )
    r
)
Map atPutNumber := method(
    self atPut(
        call evalArgAt(0) asMutable removePrefix("\"") removeSuffix("\""),
        call evalArgAt(1)
    )
)

s := File with("phonebook.json") openForReading contents
phoneNumbers := doString(s)
phoneNumbers keys println
phoneNumbers values println
// list(Bob Jack, Andy Scott)
// list(+86 18733423232, +21 1281928192)
