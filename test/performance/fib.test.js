'use strict';

/**
 * 斐波那契数列的各种实现的性能测试
 */

// #递归法 x 6,514 ops/sec ±1.41% (70 runs sampled)
// #数组缓存的递归法 x 211,086 ops/sec ±3.59% (67 runs sampled)
// #加减法 x 18,430,008 ops/sec ±1.47% (76 runs sampled)
// Fastest is #加减法

const Benchmark = require('benchmark')

const suite = new Benchmark.Suite

const util = require('../../app/util')

var fib1 = util.fib,
  fib2 = util.fib2,
  fib3 = util.fib3

var num = 20;

suite
  .add('#递归法',() => {
    fib2(num);
  })
  .add('#数组缓存的递归法',() => {
    fib3(num);
  })
  .add('#加减法',() => {
    fib1(num);
  })
  .on('cycle',event => {
    console.log(event.target + '');
  })
  .on('complete',function(){
      console.log(`Fastest is ${this.filter('fastest').map('name')}`)
  })
  .run({async:true})