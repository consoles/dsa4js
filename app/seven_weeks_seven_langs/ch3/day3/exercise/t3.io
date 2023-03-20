// 改进本节生成的XML程序，使其可处理属性：如果第一个参数是映射（用大括号语法），
// 则为XML程序添加属性。例如：
// book({"author": "Tate"}...)将打印出<book author="Tate">

OperatorTable addAssignOperator(":", "atPutNumber")

curlyBrackets := method(
  r := Map clone
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


Builder := Object clone

first := true

Builder forward := method(
  write("<", call message name)
  call message arguments foreach(
    arg,
    if (first,
      if(arg name == "curlyBrackets",
        first := true
        attr := self doMessage(arg)
        attr foreach(k, v, write(" " .. k .. "=\"" .. v .. "\"")))
      ">" println
      first := false
    )
    content := self doMessage(arg)
    if (content type == "Sequence", writeln(content))
  )
  writeln("</", call message name, ">")
)

str := File with("t3.txt") openForReading contents
doString(str)

// <ul author="Tate" age="10">
// <li author="Tate">
// Io
// </li>
// <li name="game" date="2022-01-02">
// Lua
// </li>
// <li>
// JavaScript
// </li>
// </ul>
