const UF3 = require('./uf3');

/**
 * 使用路径压缩的加权quick union
 * 
 * 理想情况下：每个节点都连接到它的根节点，但是又不能像quick-find算法那样通过修改大量连接到实现这一点
 * 得到的是一颗几乎扁平化的树，是理论最优算法
 */
module.exports = class UF4 extends UF3 {
    /**
     * 检查节点的同时将其直接连接到根节点
     */
    find(p) {
        const root = super.find(p);
        let current = this.currentAccessArrayCount;
        // 路径上的所有节点直接指向根节点
        while (p !== root) {
            const parent = this.ids[p];
            this.ids[p] = root;
            this.totalAccessArrayCount += 2;
            current += 2;
            p = parent;
        }
        this.currentAccessArrayCount = current;
        return root;
    }
}