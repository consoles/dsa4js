% 考虑形如[{item quantity price}, ...]的购物列表。写一个列表解析，构建形如
% [{item total_price}, ...]的商品列表，其中total_price是quantity乘以price。

Goods = [{apple, 1, 0.45}, {banana, 2, 1.5}, {orange, 3, 4}].
GoodsTotalPrice =  [{ Product, Count, Price, Price * Count } || { Product, Count, Price } <- Goods].
% [{apple,1,0.45,0.45},{banana,2,1.5,3.0},{orange,3,4,12}]
