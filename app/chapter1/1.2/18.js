// 累加器的方差。以下代码为Accumulator类添加了var()和stddev()方法，它们计算了addDataValue()的方法的参数的方远大和标准差，验证这段代码

class Accumulator {
    constructor() {
        this.m = 0;
        this.s = 0;
        this.n = 0;
    }
    addDataValue(x) {
        const n = ++this.n;
        const m = this.m;
        this.s = this.s + (n - 1) / n * (x - m) * (x - m);
        this.m = m + (x - m) / n;
    }
    get mean() {
        return this.m;
    }
    get var() {
        return this.s / (this.n - 1);
    }
    get stddev() {
        return Math.sqrt(this.var);
    }
}

const a = new Accumulator();
for (let i = 1; i <= 5; i++) {
    a.addDataValue(i);
}
debugger