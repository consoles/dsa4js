// 令牌桶限流
// 这种算法可以处理瞬时大流量

class TokenBucket {
    constructor(apis,limitCount,limitTime){
        this.apis = apis;
        this.limitCount = limitCount;
        this.limitTime = limitTime;
        
        this.tokens = new Map();
        for(const api of this.apis) {
            this.tokens.set(api,limitCount);
        }
    }
    run(){
        setInterval(() => {
            for(const key of this.tokens.keys()) {
                this.tokens.set(key,this.limitCount)
            }
        },this.limitTime);
    }
    getToken(api) {
        if(!this.tokens.has(api)) return false;
        const count = this.tokens.get(api)
        if (count <= 0) return false
        this.tokens.set(api,count - 1);
        return true;
    }
}

const tokenBucket = new TokenBucket(['user/login','user/reg'],3,5000)
tokenBucket.run();
const util = require('util')

// util.log('start')
// setInterval(() => {
//     const success = tokenBucket.getToken('user/login');
//     util.log(success);
// },1000);

// 漏桶算法:对流量进行整形，以固定的速率进行消费
const EventEmitter = require('events');
class LeakBucket extends EventEmitter {
    constructor(limitTime,limitCount,capacity){
        super();
        this.limitTime = limitTime;
        this.limitCount = limitCount;
        this.capacity = capacity;
        this.data = [];
    }
    run(){
        setInterval(() => {
            const data = this.data.length > 0 ? this.data.shift() : null;
            this.emit('data',data);    
        },(this.limitTime / this.limitCount));
    }
    input(data){
        if (this.data.length >= this.capacity) {
            return false;
        }
        this.data.push(data);
        return true;
    }
}

// 8s允许10个请求
const lb = new LeakBucket(8000,10,5);
lb.run();

let count = 0
setInterval(() => {
    const flag = lb.input(count++)
    util.log('input',flag)
},500)

lb.on('data',data => {
    util.log('get data',data)
})