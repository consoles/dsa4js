// Speed3
/**
 * 并查集
 * 使用父指针树
 */
class Container {
    constructor() {
        this._amount = 0;
        this.parent = this; // 最初，每个容器都是它自己树的根节点
        this.size = 1;
    }

    // 最坏 O(N*lgN), 均摊 O(1)
    groupSize() {
        const root = this._findRootAndCompress();
        return root.size;
    }

    /**
     * log(N)
     */
    addWater(amount) {
        const root = this._findRootAndCompress(); // 查找根节点，并且压缩路径
        root._amount += amount / root.size; // 向根节点添加水
    }

    /**
     * 最坏情况（即：第一次调用）是 log(N)， 后续再次调用就是 O(1), 因为路径被压缩了
     * 这个方法的复杂度是树的高度，在 `connectTo` 方法中使用 link-by-size 策略可以保证树的高度最多为对数关系
     * path compression: 将遇到的每个节点都变成树的直接子节点，在遍历树的同时也修改了树，这样可以让未来的操作更加高效
     * 路径压缩并返回根节点
     * @private
     * @returns 根节点
     */
    _findRootAndCompress() {
        if (this.parent !== this) { // 检查当前容器是否是树的根节点
            this.parent = this.parent._findRootAndCompress(); // 递归查找根节点并将其设置为当前容器的父节点
        }
        return this.parent;
    }

    /**
     * log(N)
     */
    get amount() {
        const root = this._findRootAndCompress(); // 查找根节点，并且扁平化路径
        return root._amount; // 从根节点读取水量
    }

    /**
     * log(N)
     * link-by-size 策略，小树合并到大树
     * 保证树的高度最大为对数关系，例如包含 8 个容器的树的高度不能高于 3
     * 因为导致树的高度增加只有 1 种情况：2 个树的节点数量相同，合并会导致高度加 1， 节点数量翻倍
     */
    connectTo(other) {
        const root1 = this._findRootAndCompress();
        const root2 = other._findRootAndCompress();
        if (root1 === root2) return; // 这个检查是必须的
        const size1 = root1.size;
        const size2 = root2.size;
        const newAmount = (root1._amount * size1 + root2._amount * size2) / (size1 + size2);

        if (size1 <= size2) {
            root1.parent = root2
            root2._amount = newAmount;
            root2.size += size1;
        } else {
            root2.parent = root1;
            root1._amount = newAmount;
            root1.size += size2;
        }
    }
}

const a = new Container()
const b = new Container()
const c = new Container()
const d = new Container()
a.addWater(12)
d.addWater(8)
a.connectTo(b)
b.connectTo(c)
b.connectTo(d)
console.log(a.amount, b.amount, c.amount, d.amount)

// 最坏时间复杂度为 log(N)，但是使用均摊复杂度分析的话，这种实现的所有方法的复杂度都是 O(1)，性能非常好，在绝大多数情况下这种实现的性能是最优的
