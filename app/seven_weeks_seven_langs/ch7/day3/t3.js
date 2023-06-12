// ;; 接下来，我会描述一个称之为理发师问题（sleeping barber）的题目，它是由Edsger Dijkstra于1965年提出的，特点如下:

// ;; 1. 理发店接待顾客
// ;; 2. 顾客到达理发店的时间间隔随机，范围10~30毫秒
// ;; 3. 理发店的等待室里有三把椅子
// ;; 4. 理发店只有一位理发师和一把理发椅
// ;; 5. 当理发椅为空时，一位顾客坐上去，叫醒理发师，然后理发
// ;; 6. 如果所有椅子都被占用，新来的顾客就会离开
// ;; 7. 理发需要20毫秒
// ;; 8. 完成理发后，顾客会起身离开

// ;; 实现一个多线程程序来决定一个理发师在10秒内可以为多少位顾客理发

// ;; https://charlieharvey.org.uk/page/seven_languages_clojure_three

const _ = require('lodash')

class BarberShop {
    constructor() {
        this.totalBarberCount = 0 // 总理发人数计数器

        this.waitChairCount = 3 // 理发店的等待室里有三把椅子
        this.barberManCount = 1 // 理发师数量
        this.barberChairCount = 1 // 理发椅子数量

        this.isBarberManIdle = true // 理发师是否空闲
        this.waitingCustomers = [] // 坐在等待椅子上等待理发的顾客
    }

    checkCanEnter() {
        // 理发师处于空闲状态，可以进店理发
        if (this.isBarberManIdle) {
            return true
        }
        return this.waitingCustomers.length < this.waitChairCount
    }

    _sleep(ms) {
        return new Promise(resolve => setTimeout(resolve, ms))
    }

    /**
     * 顾客进店
     */
    async enter(id) {
        await this._sleep(_.random(10, 30))
        console.log(new Date(),id, '进店')
        const canEnter = this.checkCanEnter()
        if (!canEnter) {
            this.exit(id)
            return
        }
        // 检查理发师是否空闲
        if (this.isBarberManIdle) {
            await this.doBarber(id)
            return
        }
        // 检查是否能加入到等待队列
        if (this.waitingCustomers.length < this.waitChairCount) {
            this.waitingCustomers.push(id)
        }
    }

    /**
     * 顾客坐到理发椅上
     */
    _sit(id) {
        console.log(new Date(),id, '坐到理发椅上')
    }

    _handleWaitingCustomer() {
        console.log(new Date(),'waitingCustomers size:', this.waitingCustomers.length)
        if (this.waitingCustomers.length === 0) {
            return
        }
        if (!this.isBarberManIdle) return
        const id = this.waitingCustomers.shift()
        console.log(new Date(),'handleWaitingCustomer id:', id)
        return this.doBarber(id)
    }

    /**
     * 执行理发，理发需要 20 毫秒
     */
    async doBarber(id) {
        console.log(new Date(),id, '开始理发')
        this.isBarberManIdle = false
        this._sit(id)
        await this._sleep(20)
        console.log(new Date(),id, '结束理发')
        this.totalBarberCount++
        this.exit(id)
        this.isBarberManIdle = true

        await this._handleWaitingCustomer()
    }

    /**
     * 顾客离店
     */
    exit(id) {
        console.log(new Date(),id, '离店')
    }
}

async function main() {
    let id = 0
    const startTime = Date.now()

    const barberShop = new BarberShop()
    while (Date.now() - startTime < 10 * 1000) {
        const ids = []
        // 每次 5 个顾客并发进店
        for(let i = 0; i < 5; i++) {
            ids.push(id++)
        }
        await Promise.all(ids.map(id => barberShop.enter(id)))
    }
    console.log(new Date(),'10s 内总理发人数:', barberShop.totalBarberCount)
    // 10s 内总理发人数: 280
}

console.time('barberShop')
main().then(() => {
    console.timeEnd('barberShop')
}).catch(console.error)
