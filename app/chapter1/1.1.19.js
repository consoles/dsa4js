const fib1 = n => n <= 1 ? n : fib1(n - 1) + fib1(n - 2);

let ret = fib1(10);
// debugger

// 使用记忆化搜索
const fib2 = n => {
    const m = [];
    const _fib = num => {
        if (m[num] !== void 0) return m[num];
        if (num <= 1) return m[num] = num;
        return m[num] = _fib(num - 1) + _fib(num - 2);
    }

    const ret = _fib(n);
    // debugger
    return ret;
}

ret = fib2(10);
// debugger

// DP
const fib3 = n => {
    if (n <= 1) return n;
    let a = 0, b = 1;
    let ret = 0;
    for (let i = 2; i <= n; i++) {
        ret = a + b;
        a = b;
        b = ret;
    }
    return ret;
}

ret = fib3(10);
// debugger

console.time('fib1');
fib1(43);
console.timeEnd('fib1');

console.time('fib2');
fib2(43);
console.timeEnd('fib2');

console.time('fib3');
fib3(43);
console.timeEnd('fib3');