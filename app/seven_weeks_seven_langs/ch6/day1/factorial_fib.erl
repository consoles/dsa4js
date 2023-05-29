-module(factorial_fib).
-export([factorial/1]).
-export([fib/1]).

factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).

fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

% 打开 erl 使用 c 命令编译，然后运行
% $ erl
% Eshell V13.2.2  (abort with ^G)
% 1> c(factorial_fib).
% {ok,factorial_fib}
% 2> factorial_fib:factorial(5).
% 120
% 3> factorial_fib:fib(5).
% 5
