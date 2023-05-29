-module(doctor).
-export([loop/0]).

% 监控轮盘赌进程，轮盘赌进程退出的时候打印日志并创建一个新的轮盘赌进程
loop() ->
    process_flag(trap_exit, true),
    receive
        new ->
            io:format("Creating and monitoring process.~n"),
            % 利用 spawn_link 创建一个新的进程并把进程链接起来
            % 这样当 roulette 进程中止的时候，doctor 进程就能获得通知
            % 我们注册了那个新产生进程的PID，把它和revolver原子关联起来。
            % 现在，用户可以用revolver ! message这种形式发送消息给新产生的进程。
            register(revolver, spawn_link(fun roulette:loop/0)),
            loop();

        {'EXIT', From, Reason} -> 
            io:format("Process ~p died with reason ~p.", [From, Reason]),
            io:format(" Restarting. ~n"),
            self() ! new,
            loop()
    end.

% c(doctor).
% Doctor = spawn(fun doctor:loop/0).
% <0.132.0>

% revolver ! 1.
% ** exception error: bad argument
%      in operator  !/2
%         called as revolver ! 1

% 创建并注册进程
% Doctor ! new.
% Creating and monitoring process.
% new

% 给进程发消息
% revolver ! 1.
% click.
% 1
% revolver ! 3.
% bang.
% Process <0.86.0> died with reason {roulette,die,at,{12,6,43}}.3
%  Restarting.
% Creating and monitoring process.
% revolver ! 4.
% click.
% 4
