// Speed1
class Group{
    constructor(container) {
        this.amountPerContainer = 0;
        this.members = new Set();
        this.members.add(container);
    }
}
class Container {
    constructor() {
        this.group = new Group(this);
    }
    get amount() {
        return this.group.amountPerContainer;
    }
    // O(1)
    groupSize() {
        return this.group.members.size
    }
    // O(1)
    flush() {
        this.group.amountPerContainer = 0;
        this.group.members.clear();
        this.group.members.add(this);
    }
    /**
     * O(1) 复杂度
     * 优化了所有容器中都存储有自己的 amount 改为 所有相连容器都指向同一个 group，amount 只存储一次
     */
    addWater(amount) {
        const amountPerContainer = amount / this.group.members.size;
        this.group.amountPerContainer += amountPerContainer;
    }
    /**
     * O(N)
     * 还是需要遍历组中的所有容器
     */
    connectTo(other) {
        if (this.group === other.group) return
        const size1 = this.group.members.size
        const size2 = other.group.members.size
        const to1 = this.amount * size1
        const to2 = other.amount * size2
        const newAmount = (to1 + to2) / (size1 + size2)
        for (const c of other.group.members) {
            this.group.members.add(c)
        }
        for (const c of other.group.members) {
            c.group = this.group
        }
        for (const c of this.group.members) {
            c.group.amountPerContainer = newAmount
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
