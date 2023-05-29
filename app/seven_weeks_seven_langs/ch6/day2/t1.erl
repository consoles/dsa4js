% 考虑包含键－值元组的列表，如[{erlang, "a functional language"}, {ruby, "an OO language"}]。写一个函数，接受列表和键为参数，返回该键对应的值

-module(t1).
-export([lookup/2]).

lookup(List, Key) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Value} -> Value;
        false -> not_found
    end.

% c(t1).
% t1:lookup([{erlang, "a functional language"}, {ruby, "an OO language"}], ruby).
% "an OO language"
%  t1:lookup([{erlang, "a functional language"}, {ruby, "an OO language"}], erlang).
% "a functional language"
%  t1:lookup([{erlang, "a functional language"}, {ruby, "an OO language"}], php).
% not_found
