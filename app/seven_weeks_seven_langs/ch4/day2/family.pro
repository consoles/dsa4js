father(zeb, john_boy_sr).
father(john_boy_sr, john_boy_jr).

ancestor(X, Y) :- 
 	father(X, Y).
ancestor(X, Y) :- 
    father(X, Z), ancestor(Z, Y).

% father是实现递归子目标的核心事实
% 规则ancestor/2有两个子句.如果一个规则由多个子句组成，那么其中一个子句为真，则这个规则为真。
% 把子句间的逗号看成是条件“与”的关系，把子句之间的句号看成是条件“或”的关系。
% 第一个子句表明 如果X是Y的father，那么X是Y的ancestor。
% 第二个子句表明如果我们可以证明X是Z的father并且同一个Z是Y的一个ancestor，那么X就是Y的一个 ancestor。

% | ?- ancestor(john_boy_sr, john_boy_jr). => true
% | ?- ancestor(zeb, john_boy_jr). => true

% 可以在查询中使用变量：谁是 john_boy_jr 的祖先
% | ?- ancestor(Who, john_boy_jr). 
% Who = john_boy_sr ? a
% Who = zeb
% no
% | ?- 

% 这个谓词也能返回来用：zeb 是谁的祖先
% | ?- ancestor(zeb, Who).
% Who = john_boy_sr ? a
% Who = john_boy_jr
% no
% | ?- 

% 以上知识库可以实现 2 个目的：寻找祖先和后代
