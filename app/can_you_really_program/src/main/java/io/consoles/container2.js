class Container {
    constructor() {
        this.amount = 0;
        this.containers = new Set();
        this.containers.add(this)
    }
    addWater(amount) {
        const p = amount / this.containers.size;
        for (const container of this.containers) {
            container.amount += p;
        }
    }
    connectTo(other) {
        this.containers.add(other)
        for (const c of other.containers) {
            c.containers.add(this)
        }
        let total = 0
        for (const c of this.containers) {
            total += c.amount
        }
        const p = total / this.containers.size
        for (const c of this.containers) {
            c.amount = p
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
