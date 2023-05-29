% 列表解析
Fibs = [1, 1, 2, 3, 5].
Double = fun(X) -> X * 2 end.

lists:map(Double, Fibs).
% [2,2,4,6,10]

% 使用更简单的 列表解析 语法
[Double(X) || X <- Fibs].
% [2,2,4,6,10]
% 更简单的语法
[X * 2 || X <- Fibs].
% [2,2,4,6,10]

% 商品
Goods = [{apple, 1, 0.45}, {banana, 2, 1.5}, {orange, 3, 4}].
% 1 块钱商品收 8 分税
WithTax = [{ Product, Count, Price, Price * Count * 0.08 } || { Product, Count, Price } <- Goods].
% [{apple,1,0.45,0.036000000000000004},
%  {banana,2,1.5,0.24},
%  {orange,3,4,0.96}]
% 向会员卡用户展示打 5 折的商品
WithDiscount = [{ Product, Price, Price * 0.05 } || { Product, _, Price } <- Goods].
% [{apple,0.45,0.022500000000000003},
%  {banana,1.5,0.07500000000000001},
%  {orange,4,0.2}]

% 返回X，X从[1, 2, 3, 4]中取值，小于4且大于1
[X || X <- [1, 2, 3, 4], X < 4, X > 1].
% [2, 3]
% X = [1, 2], Y = [3, 4]，计算笛卡尔积
[{X, Y} || X <- [1, 2, 3, 4], X < 3, Y <- [5, 6]].
% [{1,5},{1,6},{2,5},{2,6}]

