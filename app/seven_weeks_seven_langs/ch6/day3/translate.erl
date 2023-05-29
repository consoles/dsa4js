-module(translate).
-export([loop/0]).

loop() ->
    receive
        "casa" -> 
            io:format("house~n"),
            loop();

        "blanca" ->
            io:format("white~n"),
            loop();

        _ ->
            io:format("I don't understand~n"),
            loop()
    end.

% c(translate).
% 使用 spawn 函数产生一个进程, 进程 id 是 <0.82.0>
% Pid = spawn(fun translate:loop/0).
% <0.82.0>
% 发送消息：Pid ! message
% Pid ! "casa".
% house
% "casa"
% Pid ! "blanca".
% white
% "blanca"
% Pid ! "loco".
% I don't understand
% "loco"
