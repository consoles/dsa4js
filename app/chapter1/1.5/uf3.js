const UF2 = require('./uf2');

/**
 * 加权quick union
 * 
 * O(lgN):处理N个触点M条连接最多访问数据cMlgN次
 * 
 * 想办法平衡quick union中树的深度，防止退化成链表
 * quick union算法中随意将一棵树连接到另一棵树，现在的实现维护树的大小，将较小的树合并到较大的树
 * 树的大小定义为树中的节点数
 * 
 * 即使这一个小小的优化也大大降低了树的高度：算法性能大幅度提高！
 * 
 * 远离根节点的节点其实非常少。事实上只含有一个节点的树被归并到更大的树中的情况非常常见，这样的节点到根节点的距离也就是一条链接而已。
 * 绝大多数连通性问题此算法都能再常数时间内解决。
 */
module.exports = class UF3 extends UF2 {
    constructor(N, desc) {
        super(N, desc);
        this.sz = new Array(N).fill(1); // 各个根节点锁对应分量的大小（树中节点的数目）
    }
    /**
     * 最坏情况：union(0,1),union(2,3),union(4,5),union(6,7),union(0,2),union(4,6),union(0,4)
     * 这些要归并的树大小总是相等的，是2^N，树的高度正好是N。归并两个含有2^N个节点的树的时候，我们得到的树含有2^N+1个节点，树的高度增加到了N+1，这个算法可以保证对数级别的性能
     */
    union(p, q) {
        const pRoot = this.find(p);
        let current = this.currentAccessArrayCount;
        const qRoot = this.find(q);
        current += this.currentAccessArrayCount;
        if (pRoot === qRoot) return;
        // 将小树合并到大树
        this.totalAccessArrayCount += 2;
        current += 2;
        if (this.sz[pRoot] < this.sz[qRoot]) {
            this.ids[pRoot] = qRoot;
            this.sz[qRoot] += this.sz[pRoot];
            this.totalAccessArrayCount += 3;
            current += 3;
        } else {
            this.ids[qRoot] = pRoot;
            this.sz[pRoot] += this.sz[qRoot];
            this.totalAccessArrayCount += 3;
            current += 3;
        }
        this.currentAccessArrayCount = current;
        this.componentCount--;
    }
}