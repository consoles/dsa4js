% 写一个函数， 用递归返回字符串中的单词数。
-module(t4).
-export([count_words/1]).
-export([count_words2/1]).

count_words(Str) -> 
    Words = string:tokens(Str, " "),
    length(Words).

% 这里运用了一个特性，字符串是列表
% 32 是空格
count_words2([]) -> 1;
count_words2([32 | Rest]) -> 1 + count_words2(Rest);
%  Warning: variable 'First' is unused
% count_words2([First|Rest]) -> 0 + count_words2(Rest).
count_words2([_|Rest]) -> 0 + count_words2(Rest).

% 打开 erl 使用 c 命令编译，然后运行
% $ erl
% Eshell V13.2.2  (abort with ^G)
% 1> c(t4).
% {ok,t4}
% 2> t4:count_words("hello world").
% 5
% 3> t4:count_words2("hello world").
% 5
