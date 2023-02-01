// Memory1,Memory2 
class Container {
    constructor() {
        this.amount = 0;
        this.group = null; // 由 HashSet 变为 ArrayList 节省内存, 将其初始化为 null，真正需要使用的时候进行初始化
    }
    addWater(amount) {
        if (this.group === null) {
            this.amount += amount; // 如果容器是孤立的，则更新自己
        } else {
            const p = amount / this.group.length;
            for (const container of this.group) {
                container.amount += p;
            }
        }
    }
    connectTo(other) {
        if (this.group === null) {
            this.group = [this] // lazy initialization
        }
        if (other.group === null) {
            other.group = [other]
        }
        if (this.group === other.group) return
        const size1 = this.group.length
        const size2 = other.group.length
        const to1 = this.amount * size1
        const to2 = other.amount * size2
        const newAmount = (to1 + to2) / (size1 + size2)
        for (const c of other.group) {
            this.group.push(c)
        }
        for (const c of other.group) {
            c.group = this.group
        }
        for (const c of this.group) {
            c.amount = newAmount
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

// 这个版本相比 Reference 版本内存占用少了 50%
// 但是可读性比较差(list 类型掩盖了 group 实际上是无序不重复集合的事实)
// 其次将不包含任何容器进行特例而进行处理是一种不必要的复杂化,其唯一的目的仅仅是节省一点内存
