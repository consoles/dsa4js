-module(basic).
% 向外暴露一个名称为 mirror 的函数，该函数有一个参数
-export([mirror/1]).

% 接收一个参数，返回值为传入的参数
mirror(Anything) -> Anything.

% 打开 erl 使用 c 命令编译，然后运行
% $ erl
% Eshell V13.2.2  (abort with ^G)
% 1> c(basic).
% {ok,basic}
% 2> mirror(hello).
% ** exception error: undefined shell command mirror/1
% 3> basic:mirror(hello).
% hello
% 4> basic:mirror(world).
% world
% 5>

% 注意要使用 `模块名:函数` 的方式调用函数
