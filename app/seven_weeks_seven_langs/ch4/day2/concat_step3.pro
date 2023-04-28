concatenate([], List, List).
% 将 List1 的第一个元素连接到 List2 前面
concatenate([Head|[]], List, [Head|List]).
% 定义 2 个规则连接长度为 2 和长度为 3 的列表
concatenate([Head1|[Head2|[]]], List, [Head1, Head2|List]).
concatenate([Head1|[Head2|[Head3|[]]]], List, [Head1, Head2, Head3|List]).

concatenate([malfoy, granger], [potter], What).
% What = [malfoy,granger,potter] ? 
% yes
