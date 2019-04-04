const UF1 = require('./uf1');

/**
 * quick union
 * 
 * 最好:O(N):unoin(1,0),unoin(2,0),unoin(3,0)
 * 最坏：O(N^2):unoin(0,1),unoin(0,2),unoin(0,3)
 * 每个触点对应的id元素都是同一分量中的另一个触点的名称（也可能是它自己）
 */
module.exports = class UF2 extends UF1 {
    /**
     * 将p和q的根节点统一
     */
    union(p, q) {
        const pRoot = this.find(p);
        let current = this.currentAccessArrayCount;
        const qRoot = this.find(q);
        current += this.currentAccessArrayCount;
        if (pRoot === qRoot) return;
        this.ids[pRoot] = qRoot;
        this.currentAccessArrayCount++;
        this.totalAccessArrayCount++;
        this.currentAccessArrayCount = current;
        this.componentCount--;
    }
    /**
     * 从给定的触点触点开始，由它的链接得到另一个触点，再由这个触点到达第三个触点，直到找到根节点（链接指向自己的触点，这样的触点必然存在）
     * 
     * 当两个触点拥有相同的根节点的时候，它们处于同一连通分量
     * 
     * 最好情况：只需1次数组访问就可以得到触点所在分量的标识符
     * 最坏情况：2N+1次数组访问，此时树退化成了链表，例如:union(0,1),union(0,2),union(0,3),树的高度为N-1，0-1-2-3-4
     */
    find(p) {
        this.currentAccessArrayCount = 0;
        while (p !== this.ids[p]) {
            p = this.ids[p];
            this.totalAccessArrayCount++;
            this.currentAccessArrayCount++;
        }
        this.totalAccessArrayCount++;
        this.currentAccessArrayCount++;
        return p;
    }
}