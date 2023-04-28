% 一种食物具有一定的类型
food_type(velveeta, cheese).
food_type(ritz, cracker).
food_type(spam, meat).
food_type(sausage, meat).
food_type(jolt, soda).
food_type(twinkie, dessert).

% 一种食物类型具有特定的味道
flavor(sweet, dessert).
flavor(savory, meat).
flavor(savory, cheese).
flavor(sweet, soda).

% 如果食物X属于Z类食物且Z也具有特有味道Y，则食物X具有food_flavor Y
food_flavor(X, Y) :- food_type(X, Z), flavor(Y, Z).

/*
| ?- ['day1/food.pl'].
compiling D:/dsa4js/app/seven_weeks_seven_langs/ch4/day1/food.pl for byte code...
D:/dsa4js/app/seven_weeks_seven_langs/ch4/day1/food.pl compiled, 16 lines read - 1561 bytes written, 4 ms

yes
| ?- food_type(What, meat).

What = spam ? ;

What = sausage ? ;

no
*/
