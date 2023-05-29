% 写一个递归计数到10的函数。
-module(t5).
-export([count_to_10/0]).

% 无参函数
count_to_10() -> count_to_10(1).

count_to_10(10) -> io:format("Counting: 10~nCounting finished.~n");
count_to_10(N) ->
    io:format("Counting: ~p~n", [N]),
    count_to_10(N + 1).

