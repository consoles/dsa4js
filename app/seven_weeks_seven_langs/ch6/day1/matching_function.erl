-module(matching_function).
-export([number/1]).

number(one) -> 1;
number(two) -> 2;
number(three) -> 3.

% 打开 erl 使用 c 命令编译，然后运行
% $ erl
% Eshell V13.2.2  (abort with ^G)
% 1> c(matching_function).
% {ok,matching_function}
% 2> matching_function:number(one).
% 1
% 3> matching_function:number(two).
% 2
% 4> matching_function:number(three).
% 3
% 5> matching_function:number(four).
% ** exception error: no function clause matching
%                     matching_function:number(four) (matching_function.erl, line 4)
