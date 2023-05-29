-module(t7).
-export([accept/1]).
-export([loop/0, translate/2]).


accept(Port) ->
    {ok, Socket} = gen_tcp:listen(Port, [binary, {active, true}, {packet, line}, {reuseaddr, true}]),
    io:format("Echo server listening on port ~p~n", [Port]),
    
    server_loop(Socket).

server_loop(Socket) ->
    {ok, Connection} = gen_tcp:accept(Socket),
    Handler = spawn(fun () -> echo_loop(Connection) end),
    gen_tcp:controlling_process(Connection, Handler),
    io:format("New connection ~p~n", [Connection]),
    server_loop(Socket).

%% Echoes the incoming lines from the given connected client socket
echo_loop(Connection) ->
    receive
        {tcp, Connection, Data} ->
        % 补充代码调用翻译服务，返回客户端翻译后的结果
        Translator = spawn(fun t7:loop/0),
        Translated = translate(Translator, binary_to_list(Data)),
        io:format("Received data: ~p~n", [Data]),
        io:format("Translated data: ~p~n", [Translated]),
	    gen_tcp:send(Connection, Translated),
	    echo_loop(Connection);
	{tcp_closed, Connection} ->
	    io:format("Connection closed ~p~n", [Connection])
    end.

loop() -> 
    receive
        {From, "casa"} ->
            From ! "house\n",
            loop();

        {From, "blanca"} ->
            From ! "white\n",
            loop();

        {From, _} ->
            From ! "I don't understand.\n",
            loop()
end.

translate(To, Word) ->
    W = re:replace(Word, "\\s+", "", [global,{return,list}]),
    io:format("Translating ~p to ~p~n", [W, To]),
    To ! {self(), W},
    receive
        Translation -> Translation
    end.

% $ erl
% Eshell V13.2.2  (abort with ^G)
% 1>  c(t7).
% {ok,t7}
% 2>  t7:accept(1234).
% Echo server listening on port 1234
% New connection #Port<0.5>
% Translating "a" to <0.88.0>
% Received data: <<"a\r\n">>
% Translated data: "I don't understand.\n"
% Translating "b" to <0.89.0>
% Received data: <<"b\r\n">>
% Translated data: "I don't understand.\n"
% Translating "c" to <0.90.0>
% Received data: <<"c\r\n">>
% Translated data: "I don't understand.\n"
% Translating "casa" to <0.91.0>
% Received data: <<"casa\r\n">>
% Translated data: "house\n"
% Translating "white" to <0.92.0>
% Received data: <<"white\r\n">>
% Translated data: "I don't understand.\n"
% Translating "d" to <0.93.0>
% Received data: <<"d\r\n">>
% Translated data: "I don't understand.\n"

% telnet localhost 1234
