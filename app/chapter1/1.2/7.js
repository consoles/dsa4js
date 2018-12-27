// 字符串反转
const fn = s => {
    const n = s.length;
    if (n <= 1) return s;
    const half = n >> 1;
    const a = s.substring(0, half);
    const b = s.substring(half, n);
    return fn(b) + fn(a);
};

const ret = fn('abcde');
debugger