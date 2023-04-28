% 实现一个汉诺塔问题。它是如何工作的？
% https://zhuanlan.zhihu.com/p/392523306

% 2 ** N - 1
% f(1) = 1
% f(n) = 2 * f(n-1) + 1
f(1,1).
f(N, R) :- N > 1, N1 is N - 1, f(N1, R1), R is 2 * R1 + 1.

% f(6, What).
% What = 63 ?
% yes
% f(3, What).
% What = 7 ?
% yes
