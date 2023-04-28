% 找到一种通过print谓词仅输出成功的解决方法的方式。它是如何工作的？

% 在 Prolog 中，可以使用 findall 谓词将所有成功的解决方案收集到一个列表中，然后使用 print/1 谓词将该列表中的所有项打印到控制台上。这样可以避免使用 not 表达式，并确保只打印成功的解决方案。

% 以下是一个示例，展示了如何使用 findall 和 print 谓词仅输出成功的解决方案

% 定义一个谓词用于查找满足条件的解决方案
my_predicate(X) :-
    % 这里是一些谓词的实现，用于检查 X 是否满足条件
    % ...

% 使用 findall 收集所有成功的解决方案
findall(X, my_predicate(X), Solutions),

% 使用 print 谓词将所有成功的解决方案打印到控制台上
print_solutions(Solutions).

% 辅助谓词，用于打印解决方案列表中的所有项
print_solutions([]).
print_solutions([Head|Tail]) :-
    write(Head), nl,
    print_solutions(Tail).

