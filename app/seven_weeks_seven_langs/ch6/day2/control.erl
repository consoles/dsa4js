% ================= case
Animal = "dog".
case Animal of
    "dog" -> underdog;
    "cat" -> undercat
end.
% underdog

case Animal of 
    "bird" -> underbird;
    _ -> something_else
end.
% something_else

% ==================== if
ProgramsTerminated = 10.
if
    ProgramsTerminated > 0 ->
        success;
    ProgramsTerminated < 0 ->
        error
end.
% success

X = 0.
if
    X > 0 -> positive;
    X < 0 -> negative
end.
% ** exception error: no true branch found when evaluating an if expression

% `if` 必须有一个子句为真，因为它其实是一个函数
if
    X > 0 -> positive;
    X < 0 -> negative;
    true -> zero
end.
% zero

% 匿名函数
Negate = fun(I) -> -I end.
Negate(1).
% -1
Negate(-1).
% 1

% 列表和高阶函数
Numbers = [1, 2, 3, 4].
lists:foreach(fun(Number) -> io:format("~p~n", [Number]) end, Numbers).
% 1
% 2
% 3
% 4
% ok
Print = fun(X) -> io:format("~p~n", [X]) end.
lists:foreach(Print, Numbers).
% 1
% 2
% 3
% 4
% ok
lists:map(fun(X) -> X + 1 end, Numbers).
% [2,3,4,5]
Small = fun(X) -> X < 3 end.
lists:filter(Small, Numbers).
% [1,2]
lists:all(Small, [0, 1, 2]).
% true
lists:any(Small, [3, 4, 5]).
% false
lists:takewhile(Small, Numbers).
% [1,2]
lists:dropwhile(Small, Numbers).
% [3,4]
lists:takewhile(Small, [1, 2, 1, 4, 1]).
% [1, 2, 1]
lists:dropwhile(Small, [1, 2, 1, 4, 1]).
% [4, 1]

lists:foldl(fun(X, Sum) -> X + Sum end, 0, Numbers).
% 10
Adder = fun(X, Sum) -> X + Sum end.
lists:foldl(Adder, 0, Numbers).
% 10
