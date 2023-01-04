class Container {
    constructor() {
        this.amount = 0;
        this.id = String(Math.random())
        this.all = new Map()
        this.all.set(this.id, this)
    }
    addWater(amount) {
        const w =  amount/ this.all.size
        for (const node of this.all.values()) {
            node.amount += w
        }
    }
    /**
     * @private
     */
    _setWater(amount) {
        return this.amount = amount;
    }
    connectTo(other) {
        const id = other.id
        if (!this.all.has(id)) {
            this.all.set(id, other)
            other.all.set(this.id, this)
            for (const node of this.all.values()) {
                if (!node.all.has(id)) {
                    node.all.set(id, other)
                }
            }
        }
        let sum = 0
        for (const node of this.all.values()) {
            sum += node.amount
        }
        const size = this.all.size
        const w = sum / size
        console.log('sum: ' + sum, 'size: ' + size, 'w:', w)
        for (const node of this.all.values()) {
            node._setWater(w)
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
// b.connectTo(d)
console.log(a.amount, b.amount, c.amount, d.amount)
