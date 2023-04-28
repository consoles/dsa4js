% 翻转一个列表中的元素次序

reverse_list([], []).
% 将每个元素添加打新列表的开头
reverse_list([Head|Tail], Reversed) :-
    reverse_list(Tail, ReversedTail),
    append(ReversedTail, [Head], Reversed).

% reverse_list([1,2,3,4,5],What).
% What = [5,4,3,2,1]

% append([1,2], [3,4], What).
% What = [1,2,3,4]
