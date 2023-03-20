// 将二维数组中所有数相加

matrix := list(list(1,2,3), list(4,5,6), list(7,8,9))
sum := 0
matrix foreach(i,val,val foreach(j, num, sum = sum + num))
sum println // 45
