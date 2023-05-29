-module(monitor).
-export([loop/0]).

% 这个监视器，负责告诉我们进程是否终止
loop() ->
    process_flag(trap_exit, true),
    receive
        {monitor, Process} -> 
            link(Process),
            io:format("Monitoring process.~n"),
            loop();

        {'EXIT', From, Reason} -> 
            io:format("Process ~p died with reason ~p.~n", [From, Reason]),
            io:format("Start another one.~n"),
            loop()
        end.

% c(monitor).
% Revolver = spawn(fun roulette:loop/0).
% <0.117.0>
% Monitor = spawn(fun monitor:loop/0).
% <0.119.0>
% Monitor ! {monitor, Revolver}.
% Monitoring process.
% {monitor,<0.117.0>}
% Revolver ! 1.
% click.
% 1
% Revolver ! 3.
% bang.
% Process <0.117.0> died with reason {roulette,die,at,{11,43,23}}.
% 3
% Start another one.
% erlang:is_process_alive(Revolver).
% false
