-module(translate_service).
-export([loop/0, translate/2]).

% 每个进程都有一个信箱，receive 结构只是把消息从队列中取出来，先用它去匹配某个函数。
% 进程利用消息传递机制，在进程间彼此通信。
% 它提供了消息的传递和封装等行为，而我们付出的代价，仅仅是失去了可变状态和继承（可以通过高阶函数来模拟）
loop() -> 
    receive
        {From, "casa"} ->
            From ! "house",
            loop();

        {From, "blanca"} ->
            From ! "white",
            loop();

        {From, _} ->
            From ! "I don't understand.",
            loop()
end.

% 请求同步服务
translate(To, Word) ->
    To ! {self(), Word},
    receive
        Translation -> Translation
    end.

% c(translate_service).
% Translator = spawn(fun translate_service:loop/0).
% translate_service:translate(Translator, "casa").
% "house"
% translate_service:translate(Translator, "blanca").
% "white"
% translate_service:translate(Translator, "hola").
% "I don't understand."
