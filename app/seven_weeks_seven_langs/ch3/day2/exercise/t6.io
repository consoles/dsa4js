// 写一个转置方法，使得原列表上的matrix get(x, y)与转置后列表的 (new_matrix get(y, x))相等。

Matrix := List clone

Matrix setup := method(x, y,
    self rows := x
    self cols := y
    for(i, 1, x * y,
        self append(nil)
    )
)

Matrix set := method(x, y, val,
    self atPut(x + y * (self rows), val)
)

Matrix get := method(x, y, val,
    self at(x + y * (self rows))
)

// 定义一个打印的辅助方法
Matrix dump := method(
    for (i, 0, self rows - 1,
        "[" print
        for (j, 0, self cols - 1,
            self get(j, i) print
            " " print
        )
        "]" println
    )
)

Matrix transpose := method(
    // 视图层面的转置
    // 转置，如果矩阵不是方阵就会重新初始化空间，这里是视图层面的转置，内部存储不变，针对 set, get 进行封装
//   self get = self getSlot("get") setArgumentNames(list(
//     self getSlot("get") argumentNames at(1),
//     self getSlot("get") argumentNames at(0)
//   ) flatten)
//   self set = self getSlot("set") setArgumentNames(list(
//     self getSlot("set") argumentNames at(1),
//     self getSlot("set") argumentNames at(0),
//     self getSlot("set") argumentNames at(2)
//   ) flatten)

// 内部存储空间上的转置
    tempMatrix := self clone
    rows := tempMatrix cols
    cols := tempMatrix rows

    self empty
    // self println
    // self rows print
    // self cols println
    self setup(rows, cols)
    // self rows print
    // self cols println
    // self println
    for (i, 0, rows - 1,
        for (j, 0, cols - 1,
            v := tempMatrix get(j, i)
            // for debug

            // "set (" print
            // i print
            // ", " print
            // j print
            // "): " print
            // v println    
            // "(" + i + ", " + j + "): " + v println
            self set(i, j, v)
        )
    )
)

matrix := Matrix clone
matrix setup(3, 3) // 初始化一个 3 * 3 的矩阵
matrix set(0, 0, 1)
matrix set(1, 0, 2)
matrix set(2, 0, 3)
matrix set(0, 1, 4)
matrix set(1, 1, 5)
matrix set(2, 1, 6)
matrix set(0, 2, 7)
matrix set(1, 2, 8)
matrix set(2, 2, 9)
matrix dump
// [1 2 3 ]
// [4 5 6 ]
// [7 8 9 ]
// [nil nil nil ]
"------" println
matrix transpose
matrix dump
// [1 4 7 ]
// [2 5 8 ]
// [3 6 9 ]
