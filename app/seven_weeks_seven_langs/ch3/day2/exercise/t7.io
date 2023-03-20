// 将矩阵写入文件，并从文件中读取矩阵

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

"save matrix.txt" println
file := File with("matrix.txt")
file remove
file openForUpdating
meta := list(matrix rows, matrix cols)
file write(meta join(", "), "\n", matrix join(", "))
file close

"parse matrix from matrix.txt" println
file := File with("matrix.txt")
file openForReading
lines := file readLines
file close

metas := lines at(0) split(", ")
newMatrix := Matrix clone
newMatrix setup(metas at(0) asNumber, metas at(1) asNumber)
data := lines at(1) split(", ")
data foreach(i, v, newMatrix atPut(i, v asNumber))
newMatrix dump
