-module(double).
-export([double_all/1]).

double_all([]) -> [];
double_all([First|Rest]) -> [First + First|double_all(Rest)].

% 打开 erl 使用 c 命令编译，然后运行
% $ erl
% Eshell V13.2.2  (abort with ^G)
% 1> c(double).
% {ok,double}
% 2> double:double_all([1,2,3]).
% [2,4,6]
