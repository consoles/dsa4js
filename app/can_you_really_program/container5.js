/**
 * 使用循环链表来表示容器组。
 * 一个空的循环链表不包含任何节点，而对于只有一个节点的链表，该节点指向它自己
 */
class Container {
    constructor() {
        this._amount = 0;
        this.next = this;
    }

    /**
     * O(1)
     */
    addWater(amount) {
        this._amount += amount;
    }

    /**
     * @private
     * 重新分配水
     */
    _updateGroup() {
        let current = this;
        let totalAmount = 0;
        let groupSize = 0;
        // 计算容器数量和水量
        do {
            totalAmount += current._amount;
            groupSize++;
            current = current.next;
        } while (current != this);
        const newAmount = totalAmount / groupSize;
        // 更新容器的水量
        do {
            current._amount = newAmount;
            current = current.next;
        } while (current != this);
    }

    /**
     * O(N)
     * 懒计算
     * 虽然 `connectTo` 方法被优化成了常数时间，但是取值的时候要为节省的时间付出代价
     */
    get amount() {
        this._updateGroup()
        return this._amount
    }

    /**
     * O(1)
     * 合并 2 个循环链表只需要常数时间
     * 交换当前实例和 other 的 next 字段
     * 
     * 因为只有在调用 `getAmount` 的时候水量才是可见的，所以可以将更新 “推迟” 到下一次调用 `getAmount` 的时候
     */
    connectTo(other) {
        const oldNext = this.next;
        this.next = other;
        other.next = oldNext;
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
