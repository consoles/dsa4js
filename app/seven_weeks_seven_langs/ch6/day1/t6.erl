% 写一个函数，在给定输入为{error, Message}或success的条件下，利用匹配相应地打印出"success"或"error: message"
-module(t6).
-export([print_result/1]).

print_result({error, Message}) ->
    io:format("error: ~p~n", [Message]);
print_result({success, Message}) ->
    io:format("success: ~p~n", [Message]).

% c(t6).
% t6:print_result({error, "Not Found"}).
% error: "Not Found"
% t6:print_result({success, "Ok"}).
% success: "Ok"
