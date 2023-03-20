// 列表和映射

toDos := list("php","java","python")
toDos size println // 3
toDos append("C#")
toDos println // list(php, java, python, C#)
"----------" println
list(1,2,3,4) average println
list(1,2,3,4) sum println
list(1,2,3,4) at(1) println
list(1,2,3) append(4) println
list(1,2,3) prepend(0) println

elvis := Map clone
elvis atPut("math", 98) 
elvis at("math") println # 98
elvis atPut("english", 66)
elvis asObject println
elvis asList println // list(list(english, 66), list(math, 98))
elvis keys println // list(english, math)
elvis size // 2
