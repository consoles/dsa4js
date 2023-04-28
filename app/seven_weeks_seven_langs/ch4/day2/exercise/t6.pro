% 找出列表中最小的元素

% 基本情况是当列表只有一个元素时，该元素即为最小值
min_list([Min], Min).
% 递归情况是将列表分为头部和尾部，然后通过递归调用 min_list 谓词来找到尾部列表中的最小值。然后，使用 min 内置谓词来比较头部元素和尾部列表的最小值，从而得到列表的最小元素。
min_list([Head|Tail], Min) :-
    min_list(Tail, TailMin),
    Min is min(Head, TailMin).

% min_list([5, 3, 8, 1, 7], Min).
