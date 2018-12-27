// 如果字符串 s 中的字符循环移动任意位置之后能够得到另一个字符串 t，那么 s 就被称为 t 的回环变位（circular rotation）。例如，ACTGACG就是TGACGAC的一个回环变位，反之亦然。判定这个条件在基因组序列的研究中是很重要的。编写一个程序检查两个给定的字符串 s 和 t 是否互为回环变位。提示：答案只需要一行用到indexOf() ，length() 和字符串连接的代码。

const fn1 = (s, t) => {
    for (let i = 0; i < s.length; i++) {
        const left = s.substring(0, i);
        const right = s.substring(i);
        if (right + left === t) {
            return true;
        }
    }
    return false;
};

const fn2 = (s, t) => {
    for (let i = 0; i < s.length; i++) {
        if (s === t) {
            return true;
        }
        s = s.substring(1) + s.charAt(0);
    }
    return false;
};

const fn3 = (s, t) => s.length === t.length && s.repeat(2).includes(t);

const ret = fn3('ACTGACG', 'TGACGAC');
debugger