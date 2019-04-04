// 随机网格生成器。
// 编写一个程序 RandomGrid，从命令行接受一个 int 值 N，
// 生成一个 N×N 的网格中的所有连接。
// 它们的排列随机且方向随机（即 (p q) 和 (q p) 出现的可能性是相等的），
// 将这个结果打印到标准输出中。
// 可以使用 RandomBag 将所有连接随机排列（请见练习 1.3.34），
// 并使用如右下所示的 Connection 嵌套类来将 p 和 q 封装到一个对象中。
// 将程序打包成两个静态方法：
// generate()，接受参数 N 并返回一个连接的数组；
// main()，从命令行接受参数 N，
// 调用 generate()，遍历返回的数组并打印出所有连接。

const shuffle = arr => {
    for (let i = arr.length - 1; i > 0; i--) {
        let j = Math.floor(Math.random() * (i + 1));
        [arr[i], arr[j]] = [arr[j], arr[i]];
    }
};

class RandomBag {

    constructor() {
        this.data = [];
        this.count = 0;
    }

    isEmpty() {
        return this.count === 0;
    }

    size() {
        return this.count;
    }

    add(item) {
        this.data[this.count++] = item;
    }

    [Symbol.iterator]() {
        shuffle(this.data);
        const arr = this.data;
        const len = this.size();
        let index = 0;
        return {
            next() {
                return index < len ? { done: false, value: arr[index++] } : { done: true };
            }
        };
    }
}

class Connection {
    constructor(p, q) {
        this.p = p;
        this.q = q;
    }
}

class RandomGrid {
    static generate(N) {
        const bag = new RandomBag();
        // 横向连接
        let p = 0, q = 1;
        let i = 0;
        while (p < N * N && q < N * N) {
            if (q - p === 1) {
                // console.log(p, q);
                const conn = Math.random() > .5 ? new Connection(p, q) : new Connection(q, p);
                bag.add(conn);
            } else {
                break;
            }
            p++;
            q++;
            if (p === (i + 1) * N - 1) {
                p = ++i * N;
                q = p + 1;
            }
        }

        // 纵向连接
        p = 0, q = N;
        i = 0;
        while (p < N * N && q < N * N) {
            if (q - p === N) {
                const conn = Math.random() > .5 ? new Connection(p, q) : new Connection(q, p);
                bag.add(conn);
            } else {
                break;
            }
            p++;
            q++;
            if (p === (i + 1) * N) {
                p = ++i * N;
                q = (i + 1) * N;
            }
        }

        return bag;
    }
    static main(N) {
        const bag = RandomGrid.generate(N);
        for (const item of bag) {
            console.log(item);
        }
    }
}

// 0 1 2 
// 3 4 5
// 6 7 8

// 横向(0,1),(1,2),(3,4),(4,5),(6,7),(7,8)
// 纵向(0,3),(1,4),(2,5),(3,6),(4,7),(5,8)
RandomGrid.main(3);