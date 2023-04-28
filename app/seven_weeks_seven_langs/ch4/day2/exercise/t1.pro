% 斐波那契数列和阶乘的实现。它们是怎么工作的
% https://www.coder.work/article/6290019
fact(0,1).
fact(N, R) :- N > 0, N1 is N - 1, fact(N1, R1), R is R1 * N.

% fact(6, What).
% What = 720 ?
% yes

fib(0,0).
fib(N, R) :- N > 0, N1 is N - 1, fib(N1, R1), R is R1 + N.

% fib(6, What).
% What = 21 ?
% yes
