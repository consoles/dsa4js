// 在正文的加权 quick - union 算法示例中，对于输入的每一对整数（包括对照输入和最坏情况下的输入），给出 id[] 和 sz[] 数组的内容以及访问数组的次数。

class UF {
    constructor(N) {
        const ids = [];
        const sz = [];
        for (let i = 0; i < N; i++) {
            ids.push(i);
            sz.push(1);
        }
        this.ids = ids;
        this.sz = sz;
        this.idsCount = 0;
        this.szCount = 0;
    }
    union(p, q) {
        this.idsCount = 0;
        this.szCount = 0;
        const pRoot = this.find(p);
        let idsCount = this.idsCount;
        const qRoot = this.find(q);
        idsCount += this.idsCount;
        if (pRoot === qRoot) return;

        if (this.sz[pRoot] > this.sz[qRoot]) {
            this.ids[q] = pRoot;
            this.sz[pRoot] += this.sz[qRoot];
        } else {
            this.ids[p] = qRoot;
            this.sz[qRoot] += this.sz[pRoot];
        }
        this.szCount += 4;
        idsCount++;
        this.idsCount = idsCount;
    }
    find(p) {
        this.idsCount = 0;
        this.idsCount++;
        while (p !== this.ids[p]) {
            p = this.ids[p];
            this.idsCount++;
        }
        return p;
    }
    connected(p, q) {
        const pRoot = this.find(p);
        const qRoot = this.find(q);
        return pRoot === qRoot;
    }
}

let pairs = [
    [4, 3],
    [3, 8],
    [6, 5],
    [9, 4],
    [2, 1],
    [8, 9],
    [5, 0],
    [7, 2],
    [6, 1],
    [1, 0],
    [6, 7]
];

let uf = new UF(10);
for (const [p, q] of pairs) {
    uf.union(p, q);
    console.log(`union ${p} - ${q},id Count = `, uf.idsCount, 'sz Count = ', uf.szCount);
}
console.log();

pairs = [
    [0, 1],
    [2, 3],
    [4, 5],
    [6, 7],
    [0, 2],
    [4, 6],
    [0, 4]
];
uf = new UF(10);
for (const [p, q] of pairs) {
    uf.union(p, q);
    console.log(`union ${p} - ${q},id Count = `, uf.idsCount, 'sz Count = ', uf.szCount);
}

// union 4 - 3,id Count =  3 sz Count =  4
// union 3 - 8,id Count =  3 sz Count =  4
// union 6 - 5,id Count =  3 sz Count =  4
// union 9 - 4,id Count =  4 sz Count =  4
// union 2 - 1,id Count =  3 sz Count =  4
// union 8 - 9,id Count =  2 sz Count =  0
// union 5 - 0,id Count =  3 sz Count =  4
// union 7 - 2,id Count =  4 sz Count =  4
// union 6 - 1,id Count =  4 sz Count =  4
// union 1 - 0,id Count =  4 sz Count =  4
// union 6 - 7,id Count =  2 sz Count =  0

// union 0 - 1,id Count =  3 sz Count =  4
// union 2 - 3,id Count =  3 sz Count =  4
// union 4 - 5,id Count =  3 sz Count =  4
// union 6 - 7,id Count =  3 sz Count =  4
// union 0 - 2,id Count =  5 sz Count =  4
// union 4 - 6,id Count =  5 sz Count =  4
// union 0 - 4,id Count =  5 sz Count =  4