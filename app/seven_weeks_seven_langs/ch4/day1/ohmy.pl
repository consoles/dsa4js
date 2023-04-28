cat(lion).
cat(tiger).

dorothy(X, Y, Z) :- X = lion, Y = tiger, Z = bear.
twin_cats(X, Y) :- cat(X), cat(Y).

% `=` 的意思是合一，或者说等号两侧相同
% 我们拥有 2 个事实: lion 和 tiger 都是 cat
% 我们还有 2 个简单的规则。 在规则 `dorothy/3` 中， X,Y,Z 分别为 lion,tiger, bear
% 在规则 `twin_cats/2` 中， X 和 Y 都是 cat

% 执行一个不带参数的简单查询
/*
| ?- dorothy(lion, tiger, bear).

yes
*/
% 合一的意思是 “找出那些使规则两侧匹配的值”, 右侧 X,Y,Z 分别为 lion,tiger, bear 这些值和左侧的值是匹配的，所以合一是成功的。Prolog 报告 yes

/*
| ?- dorothy(One, Two, Thess).

One = lion
Thess = bear
Two = tiger

yes
*/
% 这个例子多了一个间接层。在子目标中，Prolog使得X、Y和Z分别与lion、tiger和bear合一。在左侧，Prolog使得X、Y和Z分别与One、Two和Three合一，然后报告结果。

% 最后那条规则twin_cats/2。这条规则说如果你能证明X和Y都是cat，那么这条规则就为真。
/*
| ?- twin_cats(One, Two).

One = lion
Two = lion ? a // 输入 a 可以得到剩下的所有回答，输入分号可以得到逐个回答

One = lion
Two = tiger

One = tiger
Two = lion

One = tiger
Two = tiger

(31 ms) yes
| ?- 
*/
