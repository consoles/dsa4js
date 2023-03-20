// 改进本节生成的XML程序，增加空格以显示缩进结构。

Builder := Object clone
Builder forward := method(
    if (call message name == "li", write("  "))
    writeln("<", call message name, ">")
    call message arguments foreach(
        arg,
        content := self doMessage(arg);
        if (content type == "Sequence", if (call message name == "li", write("    ")); writeln(content))
    )
    if (call message name == "li", write(" "))
    writeln("</", call message name, ">")
)

Builder ul(
    li("Io"),
    li("Lua"),
    li("Javascript")
)

// <ul>
//   <li>
//     Io
//  </li>
//   <li>
//     Lua
//  </li>
//   <li>
//     Javascript
//  </li>
// </ul>
