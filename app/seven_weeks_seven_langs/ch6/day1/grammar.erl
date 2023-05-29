% 以下语句可以直接在 erlang 的 repl 中执行

% this is comment
% 英文句号表示语句结束

% 字符串、整数、浮点数

% 2
2 + 2. 

% 4.0
2 + 2.0.

% "hello world"
"hello world".

% 列表
[1,2,3]. % [1,2,3] %

% 字符串其实是列表
% "Ha Ha Ha "
[72, 97, 32, 72, 97, 32, 72, 97, 32].

% 大写字母开头是是变量，小写字母开头的是原子，原子是不可变的。
% exception error: no match of right hand side value 1
v=1.
V=1. % 变量赋值 %
V. % 取变量 %
% 变量只能被赋值 1 次 
V=2. % exception error: no match of right hand side value 2 %

% 原子、列表、元组
% red 和 blue 都是原子，这些原子的名称是随意的
red.
Pill = blue.
Pill. % blue %

% 列表是异质和变长的，元组是定长的异质列表
List = [1, 2, "three"].
Origin = {0, "one"}.

% 映射
% 以原子作为键，字符串作为值
{ name, "node.js" }.
{ book, { name, "node.js" }, { author, "Scott Jack" } }.

% 模式匹配
Person = { person, { name, "Scott Jack" }, { like, "English" } }.
{ person, { name, Name}, { like, Like } } = Person.
% 
Name. 
Like.

[Head | Tail] = [1, 2, 3].
Head. % 1 %
Tail. % [2, 3] %
[One, Two|Rest] = [1, 2, 3].
One. % 1 %
Two. % 2 %
Rest. % [3] %

% 位匹配
W = 1.
X = 2.
Y = 3.
Z = 4.
% 打包：在下面的例子中，其构造器表示变量W占3位、X占3位、Y占5位、Z占5位
All = <<W:3, X:3, Y:5, Z:5>>. % <<"(d">> %
% 解包：
<<A:3, B:3, C:5, D:5>> = All.
A. % 1 %
D. % 4 %
