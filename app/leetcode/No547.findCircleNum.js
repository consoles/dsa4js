class UF {
    constructor(n) {
        const sz = new Array(n);
        for (let i = 0; i < n; i++) {
            sz[i] = i;
        }
        this.count = n; // 联通分量的个数
        this.sz = sz;
    }
    union(p, q) {
        if (p === q) {
            return;
        }
        const pId = this.find(p);
        const qId = this.find(q);
        if (pId === qId) {
            return;
        }
        for (let i = 0; i < this.sz.length; i++) {
            if (this.sz[i] === qId) {
                this.sz[i] = pId;
            }
        }
        this.count--;
    }
    // 使用quick find算法
    find(p) {
        return this.sz[p];
    }
    // 判断p和q是否是同一个联通分量
    isConnected(p, q) {
        const pId = this.find(p);
        const qId = this.find(q);
        return pId === qId;
    }
}

/**
 * @param {number[][]} isConnected
 * @return {number}
 */
var findCircleNum = function (isConnected) {
    // 一看就是求联通分量的个数，首先想到是并查集
    // const n = isConnected.length;
    // const uf = new UF(n);
    // for (let i = 0; i < n; i++) {
    //     for (let j = 0; j < n; j++) {
    //         if (isConnected[i][j] === 1) {
    //             uf.union(i, j);
    //         }
    //     }
    // }
    // return uf.count;

    // 再用dfs搞一波
    // const n = isConnected.length;
    // const visited = new Array(n).fill(false);
    // let cnt = 0;
    // function dfs(i) {
    //     visited[i] = true;
    //     for (let j = 0; j < n; j++) {
    //         if (isConnected[i][j] === 1 && !visited[j]) {
    //             dfs(j);
    //         }
    //     }
    // }
    // for (let i = 0; i < n; i++) {
    //     if (!visited[i]) {
    //         cnt++;
    //         dfs(i);
    //     }
    // }
    // return cnt;

    // 太爽了，再来BFS一把梭。。
    const n = isConnected.length;
    const visited = new Array(n).fill(false);
    let cnt = 0;
    const q = [];
    for (let i = 0; i < n; i++) {
        if (visited[i]) continue;
        cnt++;
        q.push(i);
        while (q.length) {
            const j = q.shift();
            visited[j] = true;
            for (let k = 0; k < n; k++) {
                if (isConnected[j][k] && !visited[k]) {
                    q.push(k);
                }
            }
        }
    }
    return cnt;
};

const isConnected = [[1, 0, 0], [0, 1, 0], [0, 0, 1]];
const count = findCircleNum(isConnected);
debugger;
