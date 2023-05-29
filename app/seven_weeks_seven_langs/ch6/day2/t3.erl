% 加分题：读取一个大小为9的列表或元组，表示井字棋（tic-tac-toe）的棋盘。若胜负已定，
% 则返回胜者（x或o）；若再没有可走的棋着，则返回cat；若两方都还没赢，则返回 no_winner。

% 参见 ch5/day1/t4.scala
% https://github.com/laurenipsum365/TicTacToe-Erlang/blob/master/game.erl

-module(t3).
-export([check_winner/1]).

check_winner(Board) ->
    case Board of
% 行
      {x, x, x,
       _, _, _,
       _, _, _} ->  x;

      {_, _, _,
       x, x, x,
       _, _, _} ->  x;

      {_, _, _,
       _, _, _,
       x, x, x} ->  x;

      {o, o, o,
       _, _, _,
       _, _, _} ->  o;

      {_, _, _,
       o, o, o,
       _, _, _} ->  o;

      {_, _, _,
       _, _, _,
       o, o, o} ->  o;

% 列
      {x, _, _,
       x, _, _,
       x, _, _} ->  x;

      {_, x, _,
       _, x, _,
       _, x, _} ->  x;

      {_, _, x,
       _, _, x,
       _, _, x} ->  x;

      {o, _, _,
       o, _, _,
       o, _, _} ->  o;

      {_, o, _,
       _, o, _,
       _, o, _} ->  o;
      
      {_, _, o,
       _, _, o,
       _, _, o} ->  o;
       
% 对角（反对角）
      {x, _, _,
       _, x, _,
       _, _, x} ->  x;

      {_, _, x,
       _, x, _,
       x, _, _} ->  x;

      {o, _, _,
       _, o, _,
       _, _, o} ->  o;

      {_, _, o,
       _, o, _,
       o, _, _} ->  o;

      {
        A, B, C,
        D, E, F,
        G, H, I
      } when A =/= "", B =/= "", C =/= "",
             D =/= "", E =/= "", F =/= "",
             G =/= "", H =/= "", I =/= "" ->
        cat;
      _ -> no_winner
    end.

% 测试所有行连续三个 x 的情况
t3:check_winner({x, x, x, "", "", "", "", "", ""}). % 应返回 x
t3:check_winner({"", "", "", x, x, x, "", "", ""}). % 应返回 x
t3:check_winner({"", "", "", "", "", "", x, x, x}). % 应返回 x

% 测试所有列连续三个 o 的情况
t3:check_winner({o, "", "", o, "", "", o, "", ""}). % 应返回 o
t3:check_winner({"", o, "", "", o, "", "", o, ""}). % 应返回 o
t3:check_winner({"", "", o, "", "", o, "", "", o}). % 应返回 o

% 测试所有对角线连续三个 x 的情况
t3:check_winner({x, "", "", "", x, "", "", "", x}). % 应返回 x
t3:check_winner({"", "", x, "", x, "", x, "", ""}). % 应返回 x

% 测试所有对角线连续三个 o 的情况
t3:check_winner({o, "", "", "", o, "", "", "", o}). % 应返回 o
t3:check_winner({"", "", o, "", o, "", o, "", ""}). % 应返回 o

% 测试所有位置都被占据但没有获胜者的情况
t3:check_winner({x, o, x, o, x, o, o, x, o}). % 应返回 cat

% 测试空棋盘的情况
t3:check_winner({"", "", "", "", "", "", "", "", ""}). % 应返回 no_winner
