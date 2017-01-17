'use strict';

/**
 * 最大公约数
 */

// #辗转相除 x 8,838,418 ops/sec ±1.30% (75 runs sampled)
// #更相减损 x 28,185 ops/sec ±1.54% (76 runs sampled)
// Fastest is #辗转相除

const Benchmark = require('benchmark')

const suite = new Benchmark.Suite

const util = require('../../app/util')

var gcd1 = util.gcd,
  gcd2 = util.gcd2

var num1 = 1234567,num2 = 1111111

suite
  .add('#辗转相除',() => {
    gcd1(num1,num2);
  })
  .add('#更相减损',() => {
    gcd2(num1,num2);
  })
  .on('cycle',event => {
    console.log(event.target + '');
  })
  .on('complete',function(){
      console.log(`Fastest is ${this.filter('fastest').map('name')}`)
  })
  .run({async:true})  