const assert = require('assert');

/**
 * quick find
 * 
 * O(N^2)
 */
module.exports = class UF1 {
    /**
     * 初始化N个触点
     */
    constructor(N, desc) {
        const ids = [];
        for (let i = 0; i < N; i++) {
            ids.push(i);
        }
        this.desc = desc;
        this.componentCount = N;
        this.ids = ids;
        this.totalAccessArrayCount = 0;
        this.currentAccessArrayCount = 0;
    }
    /**
     * 在p、q之间添加一条连接
     */
    union(p, q) {
        const pID = this.find(p);
        const qID = this.find(q);
        this.currentAccessArrayCount = 2;
        if (pID === qID) return;

        // 两次find访问数组2
        // 遍历id数组N
        // 可能改变的数组元素的个数 1~N-1

        // 复杂度N+3 ~ 2N+1
        // 假设使用此算法最终只得到了一个连通分量，则调用了N-1次union，即至少需要(N+3) * N-1 ~ N^2数组访问

        // 为什么需要遍历而不直接将ids[q]修改为何ids[p]一样？
        // union(0,1),union(2,3),union(0,2)，则ids[0] = 0,ids[1] = 0,ids[2] = 0,ids[3] = 2,但是此时(0,1,2,3)是一个连通分量
        for (let i = 0; i < this.ids.length; i++) {
            this.totalAccessArrayCount++;
            this.currentAccessArrayCount++;
            if (this.ids[i] === qID) {
                this.ids[i] = pID;
                this.totalAccessArrayCount++;
                this.currentAccessArrayCount++;
            }
        }
        this.componentCount--;
    }
    /**
     * 返回连通分量的标识符
     */
    find(p) {
        assert(p >= 0 && p < this.ids.length, `p = ${p}`);
        this.totalAccessArrayCount++;
        this.currentAccessArrayCount = 1;
        return this.ids[p];
    }
    /**
     * p、q是否处于同一连通分量
     */
    connected(p, q) {
        const pID = this.find(p);
        let count = this.currentAccessArrayCount;
        const qID = this.find(q);
        count += this.currentAccessArrayCount;
        this.currentAccessArrayCount = count;
        return pID === qID;
    }
    /**
     * 连通分量数目
     */
    count() {
        return this.componentCount;
    }
}
