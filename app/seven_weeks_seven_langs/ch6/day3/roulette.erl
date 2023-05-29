-module(roulette).
-export([loop/0]).

% send a number 1-6
loop() ->
    receive
        3 -> io:format("bang.~n"), exit({roulette, die, at, erlang:time()});
        _ -> io:format("click.~n"), loop()
end.

% 以上是一个简单的 CS 架构的轮盘赌程序，客户端是命令行，服务端是轮盘赌进程
% c(roulette).
% Gun = spawn(fun roulette:loop/0).
% Gun ! 1.
% 9> Gun ! 1.
% click.
% 1
% 10> Gun ! 2.
% click.
% 2
% 11> Gun ! 3.
% bang.
% 3 
% 12> Gun ! 4.
% 4
% 13> Gun ! 5.
% 5
% 14> Gun ! 6.
% 6
% 当向 Gun 进程发送 3 的时候进程已经自己退出了，后面发送 4,5,6 就没有反应了
% erlang:is_process_alive(Gun).
% false
