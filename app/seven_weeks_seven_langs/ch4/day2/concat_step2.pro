concatenate([], List, List).
% 将 List1 的第一个元素连接到 List2 前面
concatenate([Head|[]], List, [Head|List]).

concatenate([malfoy], [potter], What).
% What = [malfoy,potter] ? 
% yes
