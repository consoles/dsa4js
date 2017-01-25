'use strict'

/**
 * 闭区间[start,end]的抽象
 */
class Interval1D{
  constructor(start,end){
    this.start = Math.min(start,end)
    this.end = Math.max(start,end)
  }

  /**
   * 取交集
   * 例如:[1,4]和[3,8]具有交集[3,4]
   * @return {Array}
   */
  intersection(interval) {
    var start = interval.start,
    end = interval.end

    if (start > this.end || end < this.start) return false

    return new Interval1D(Math.max(start,this.start),Math.min(end,this.end))
  }
}

// var interval1 = new Interval1D(1,4)
// var interval2 = new Interval1D(3,8)

// var ret = interval1.intersection(interval2)
// console.dir(ret)

const N = 5
var intervals = []
for (let i = 0;i < N;i++){
  let interval = new Interval1D(Math.random() * 10 >> 0,Math.random() * 20 >> 0)
  intervals.push(interval)
}
console.log(intervals)
for (let i = 0;i < intervals.length;i++){
  for(let j = i + 1;j < intervals.length;j++){
    let set = intervals[i].intersection(intervals[j])
    set && console.log(set)
  }
}