// 对象、原型和继承
Vehicle := Object clone
Vehicle description := "Something to take you places"
Vehicle description := "Something to take far away"
Vehicle description print // Something to take far away
"\n" print
Vehicle slotNames print // list(type, description)
"\n" print

Car := Vehicle clone
Car type print // Car
"\n" print
Car description print // Something to take far away
"\n" print

ferrari := Car clone
ferrari slotNames print // list(), type 槽都没有了，这是因为按照惯例，类型应该以大写字母开头
"\n" print
ferrari type print // Car
"\n" print

// 方法
"----------------\n" print
method("Yes, your are right." println)
method() type println // Block
Car drive := method("Vroom" println)
ferrari drive // Vroom 如果槽是方法，调用这个槽会调用该方法
ferrari getSlot("drive") println // 获取槽中的内容
ferrari getSlot("type") println // 获取槽中的内容
ferrari proto println // 获取对象原型
Car proto println

// Lobby 是主命名空间，包含了所有的已命名对象
Lobby println 
