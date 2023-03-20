// 对二维列表写一个原型。该原型的dim(x, y)方法可为一个包含y个列表的列表分配内存，
// 其中每个列表都有x个元素，set(x, y)方法可设置列表中的值，get(x, y)方法可返回
// 列表中的值。

List2D := List clone

List2D dim := method(x, y,
    self rows := x
    self cols := y
    for (i, 1, x * y, 
        self append(nil)
    )
)

List2D set := method(x, y, val,
    self atPut(x + y * (self rows), val)
)

List2D get := method(x, y,
    self at(x + y * (self rows))
)

myList := List2D clone
myList dim(2, 3)
myList println

myList set(0, 0, "yes")
myList println

myList set(2, 1, 666)
myList println

myList get(0, 0) println
myList get(2, 1) println
