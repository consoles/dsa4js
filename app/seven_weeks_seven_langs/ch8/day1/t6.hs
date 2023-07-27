-- 编写一个列表推导来构建一个儿童乘法表。这个表应该是一个由三元组组成的列表，三元组的前两个整数元素的取值范围是1~12，第三个元素为前两个元素的乘积。

multTable = [(x, y, x*y) | x <- [1..12], y <- [1..12]]

main = do
  print multTable
