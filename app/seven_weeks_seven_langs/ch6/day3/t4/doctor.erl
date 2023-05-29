% 如果Doctor进程终止，使其重启自身
-module(doctor).
-behaviour(supervisor).
-export([loop/0]).
-export([start/0]).
-export([init/1]).
-export([start_service/0]).

loop() ->
    process_flag(trap_exit, true),
    receive
        new ->
            io:format("Creating and monitoring process.~n"),
            register(revolver, spawn_link(fun roulette:loop/0)),
            loop();

        quit -> 
            io:format("doctor process quit.~n"), 
            exit({d, die, at, erlang:time()});

        {'EXIT', From, Reason} -> 
            io:format("Process ~p died with reason ~p.", [From, Reason]),
            io:format(" Restarting. ~n"),
            self() ! new,
            loop()
    end.

start() ->
   io:format("Start doctor program~n"),
   register(d, spawn_link(doctor, loop, [])),
   {ok, whereis(d)}.

init(_) ->
   {ok, {{one_for_one, 1, 60 },
   		      [{doctor, {doctor, start, []},
			      permanent, brutal_kill, worker, [doctor]}]}}.

% Use the supervisor to monitor the service and keep it running.
start_service() ->
   io:format("Start service~n"),
   supervisor:start_link(doctor, []).

% $ erl
% Eshell V13.2.2  (abort with ^G)
% 1> c(roulette).
% {ok,roulette}
% 2> c(doctor).
% 11>    doctor:start_service().
% Start service
% Start doctor program
% {ok,<0.107.0>}
% 12>    d ! new.
% Creating and monitoring process.
% new
% 13>    revolver ! 3.
% bang.
% 3
% Process <0.110.0> died with reason {roulette,die,at,{17,20,25}}. Restarting.
% 14> Creating and monitoring process.
% 14>    revolver ! 1.
% click.
% 1
% 15>    revolver ! 2.
% click.
% 2
% 16>    revolver ! 3.
% bang.
% Process <0.113.0> died with reason {roulette,die,at,{17,20,34}}.3
%  Restarting.
% Creating and monitoring process.
% 17>    revolver ! 4.
% click.
% 4
% 18>    revolver ! 5.
% click.
% 5
% 19>
