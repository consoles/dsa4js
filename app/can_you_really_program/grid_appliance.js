// 设计 2 个类 Grid（电网）和 Appliance（电器）。
// 每个电网（或者电器）都有一个最大供电（或耗电）功率。
// 可以使用 plugInto 将电器连接到电网，也可以使用 on 和 off 实例方法来打开或者关闭电器（最初任何新的电器都是关闭的）。
// 将一个电器连接到另一个电网，会自动将其与第一个电网断开。
// 如果打开一个电器导致其电网过载，则 on 方法必须抛出异常。
// Grid 类的 residualPower 方法会返回当前电网剩余的可用功率。

class Grid {
    constructor(maxPower) {
        this.maxPower = maxPower
    }
    residualPower() {
        return this.maxPower
    }
}

class Appliance {
    constructor(maxPower) {
        this.maxPower = maxPower
        this.grid = null
        this.isOn = false
    }
    plugInto(grid) {
        this.grid = grid
    }
    on() {
        if (this.isOn) {
            return
        }
        this.isOn = true
        if (this.maxPower > this.grid.maxPower) {
            throw new Error('电网过载')
        }
        this.grid.maxPower -= this.maxPower
    }
    off() {
        if (!this.isOn) {
            return
        }
        this.isOn = false
        this.grid.maxPower += this.maxPower
    }
}

// 测试用例
const tv = new Appliance(150)
const radio = new Appliance(30)

const grid = new Grid(3000)

tv.plugInto(grid)
radio.plugInto(grid)
console.log("电网剩余功率:", grid.residualPower()) // 3000
tv.on()
console.log("电网剩余功率:", grid.residualPower()) // 2850
radio.on()
console.log("电网剩余功率:", grid.residualPower()) // 2820
