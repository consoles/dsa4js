%  Prolog也有输入/输出功能。找出可以打印输出变量的print谓词
% 在 Prolog 中，可以使用 write 谓词将变量的值打印到控制台上。

X = 42, write(X).

% 这个示例在 swi-prolog（输出字符串） 和 GNU prolog（输出ASCII） 不同
X = 42, Y = "Hello", write(X), write("   "), write(Y).
