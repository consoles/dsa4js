% 监视translate_service，并在它终止时重启它
-module(translate_service).
-behaviour(supervisor).
-export([loop/0, translate/1]).
-export([start/0]).
-export([init/1]).
-export([start_service/0]).

loop() ->
   receive
      {From, "casa"} ->
         From ! "house",
         loop();
      {From, "blanca"} ->
         From ! "white",
         loop();
      % If the Word is not recognized, shutdown the process.
      {From, M} ->
         From ! "I don't understand.",
         exit({M, " Not Understand!"})
end.

% Add an atom(translator) to do the translation
translate(Word) ->
   translator ! {self(), Word},
   receive
      Translation -> Translation
   end.

start() ->
   io:format("Start translating program~n"),
   register(translator, spawn_link(translate_service, loop, [])),
   {ok, whereis(translator)}.

% 在Erlang中，init/1是一个回调函数，它是一个supervisor或genserver进程的一部分，用于初始化该进程。当supervisor或genserver进程启动时，Erlang运行时系统将调用该进程的init/1函数，以便该进程可以进行初始化操作并返回一个描述该进程如何管理其子进程的规范。

% init/1函数的参数是一个任意值，它通常被命名为_，表示该参数未使用。

% 在supervisor中，init/1函数的返回值是一个包含进程状态和子进程规范的元组。进程状态是一个任意值，它可以包含有关该进程的任何有用信息。子进程规范是一个列表，它描述了该进程如何管理其子进程，包括子进程的名称、启动函数、重启策略等。

% 在gen_server中，init/1函数的返回值是一个包含进程状态和初始响应的元组。进程状态是一个任意值，它可以包含有关该进程的任何有用信息。初始响应是一个任意值，它会在该进程启动后立即发送给调用方。

% 在这个特定的代码中，init/1函数被用作supervisor进程的回调函数，用于指定如何管理translate_service进程。由于它不需要使用传递给它的参数，因此参数名称被定义为_。
init(_) ->
   {ok, {{one_for_one, 1, 60 },
   		      [{translate_service, {translate_service, start, []},
			      permanent, brutal_kill, worker, [translate_service]}]}}.

% Use the supervisor to monitor the service and keep it running.
start_service() ->
   io:format("Start service~n"),
   supervisor:start_link(translate_service, []).

% $ erl
% Eshell V13.2.2  (abort with ^G)
% 1>    c(translate_service).
% {ok,translate_service}
% 2>    translate_service:start_service().
% Start service
% Start translating program
% {ok,<0.87.0>}
% 3>    translate_service:translate("casa").
% "house"
% 4>    translate_service:translate("blanca").
% "white"
% 5>    translate_service:translate("bla").
% Start translating program
% "I don't understand."
% 6> =SUPERVISOR REPORT==== 25-May-2023::17:01:21.274000 ===
%     supervisor: {<0.87.0>,translate_service}
%     errorContext: child_terminated
%     reason: {"bla"," Not Understand!"}
%     offender: [{pid,<0.88.0>},
%                {id,translate_service},
%                {mfargs,{translate_service,start,[]}},
%                {restart_type,permanent},
%                {significant,false},
%                {shutdown,brutal_kill},
%                {child_type,worker}]

% 6>    translate_service:translate("blanca").
% "white"
% 7>    translate_service:translate("blanca").
% "white"
% 8>
