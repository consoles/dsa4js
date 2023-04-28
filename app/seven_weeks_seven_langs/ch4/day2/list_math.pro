% 列表与数学运算
count(0, []).
count(Count, [Head|Tail]) :- count(TailCount, Tail), Count is TailCount + 1.

% 一个空列表的总和是0，如果可以证明列表 Tail 的总和加上 Head 的值是 Total，那么列表的总和就是 Total
% 我们用证明目标和子目标的想法替换了递归过程
sum(0, []).
sum(Total, [Head|Tail]) :- sum(Sum, Tail), Total is Head + Sum.

average(Average, List) :- sum(Sum, List), count(Count, List), Average is Sum/Count.

% count(What, [1]).
% What = 1 ?
% sum(What, [1,2,3]).
% What = 6 ?
% average(What, [1,2,3,4]).
% What = 2.5 ? 
