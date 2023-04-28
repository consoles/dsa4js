% 对一个列表中的元素进行排序

sort_list(List, Sorted) :-
    permutation(List, Sorted),
    sorted(Sorted).

sorted([]).
sorted([_]).
sorted([X,Y|T]) :-
    X =< Y,
    sorted([Y|T]).

% 这个谓词的实现使用了两个谓词：permutation 和 sorted。permutation 用于生成列表的所有排列，sorted 用于检查列表是否按照升序排序。
% 具体来说，谓词 sort_list 首先生成列表的所有排列，然后检查每个排列是否已按升序排序。一旦找到一个已排序的排列，谓词将其返回为排序的列表。

% sort_list([5, 3, 8, 1, 7], Sorted).
% Sorted = [1,3,5,7,8] 
