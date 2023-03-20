// method_missing like ruby
Builder := Object clone

// 覆盖了 forward，使其能接收任意方法
Builder forward := method(
    writeln("<", call message name, ">")
    call message arguments foreach(
        arg,
        content := self doMessage(arg);
        if (content type == "Sequence", writeln(content)))
    writeln("</", call message name, ">")
)

Builder ul(
    li("Io"),
    li("Lua"),
    li("Javascript")
)

// <ul>
// <li>
// Io
// </li>
// <li>
// Lua
// </li>
// <li>
// Javascript
// </li>
// </ul>
