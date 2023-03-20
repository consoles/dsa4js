// 控制语句
// 循环

// loop("hello world" println) // 死循环

i := 1
while(i <= 10, i println; i = i + 1); "this one gose up to 10(while)" println

for(i, 1, 10, i println); "this one gose up to 10(for)" println

for(i, 1, 10, 2, i println); "this one gose up to 10(for, step = 2)" println

// 条件语句
if (true, "is true", "is false") println // is true
if (false) then("is true") else("is false") println // nil
if (false) then("is true" println) else("is false" println) println // is false

// 运算符
OperatorTable println // 查看运算符表
// OperatorTable_0xffffffff:
// Operators
//   0   ? @ @@
//   1   **
//   2   % * /
//   3   + -
//   4   << >>
//   5   < <= > >=
//   6   != ==
//   7   &
//   8   ^
//   9   |
//   10  && and
//   11  or ||
//   12  ..
//   13  %= &= *= += -= /= <<= >>= ^= |=
//   14  return

// Assign Operators
//   ::= newSlot
//   :=  setSlot
//   =   updateSlot

// To add a new operator: OperatorTable addOperator("+", 4) and implement the + message.
// To add a new assign operator: OperatorTable addAssignOperator("=", "updateSlot") and implement the updateSlot message.

// 自定义 异或 运算符 xor
OperatorTable addOperator("xor", 11)
// 穷举可能的情况
true xor := method(bool, if(bool, false, true)) // true xor true => false, true xor false => true
false xor := method(bool, if(bool, true, false)) // false xor true => true, false xor false => false
"verification xor opetators." println
true xor true println
true xor false println
false xor true println
false xor false println
