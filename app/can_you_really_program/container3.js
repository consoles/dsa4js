class Container {
    constructor() {
        this.amount = 0;
        this.group = new Set();
        this.group.add(this)
    }
    addWater(amount) {
        const p = amount / this.group.size;
        for (const container of this.group) {
            container.amount += p;
        }
    }
    connectTo(other) {
        if (this.group === other.group) return
        const size1 = this.group.size
        const size2 = other.group.size
        const to1 = this.amount * size1
        const to2 = other.amount * size2
        const newAmount = (to1 + to2) / (size1 + size2)
        for (const c of other.group) {
            this.group.add(c)
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
