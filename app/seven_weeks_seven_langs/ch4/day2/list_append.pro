append([oil],[water],[oil,water]).
% yes

append([oil],[water],[oil,slick]).
% no

% 列表构造器
append([tiny], [bubbles], What).
% What = [tiny,bubbles]
% yes

% 列表减法
append([dessert_topping], Who, [dessert_topping, floor_wax]).
% Who = [floor_wax]
% yes

append(One, Two, [apples, oranges, bananas]).
% One = []
% Two = [apples,oranges,bananas] ? a

% One = [apples]
% Two = [oranges,bananas]

% One = [apples,oranges]
% Two = [bananas]

% One = [apples,oranges,bananas]
% Two = []

% yes
