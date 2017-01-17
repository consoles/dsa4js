'use strict';

const expect = require('chai').expect

const util = require('../app/util')

var fib = util.fib,
  fib2 = util.fib2,
  fib3 = util.fib3,
  toBinaryString = util.toBinaryString,
  lg = util.lg,
  sum = util.sum,
  histogram = util.histogram,
  sample = util.sample,
  mul = util.mul,
  pow = util.pow,
  factorial = util.factorial,
  lnFactorial = util.lnFactorial,
  gcd = util.gcd,
  gcd2 = util.gcd2,
  randomString = util.randomString,
  binomial = util.binomial,
  betterBinomial = util.betterBinomial,
  sortAsc = util.sortAsc,
  unique = util.unique,
  rank = util.rank,
  count = util.count

describe('<util 工具类测试>', () => {
  it('#toBinaryString', () => {
    expect(toBinaryString(0)).to.be.equal((0).toString(2))
    expect(toBinaryString(1)).to.be.equal((1).toString(2))
    expect(toBinaryString(2)).to.be.equal((2).toString(2))
    expect(toBinaryString(3)).to.be.equal((3).toString(2))
    expect(toBinaryString(15)).to.be.equal((15).toString(2))
    expect(toBinaryString(10)).to.be.equal((10).toString(2))
  })
  it('#加减法求斐波那契数列', () => {
    expect(fib(1)).to.be.equal(0)
    expect(fib(2)).to.be.equal(1)
    expect(fib(3)).to.be.equal(1)
    expect(fib(4)).to.be.equal(2)
    expect(fib(5)).to.be.equal(3)
    expect(fib(10)).to.be.equal(34)
    expect(fib(11)).to.be.equal(55)
  })
  it('#递归法求斐波那契数列', () => {
    expect(fib2(1)).to.be.equal(0)
    expect(fib2(2)).to.be.equal(1)
    expect(fib2(3)).to.be.equal(1)
    expect(fib2(4)).to.be.equal(2)
    expect(fib2(5)).to.be.equal(3)
    expect(fib2(10)).to.be.equal(34)
    expect(fib2(11)).to.be.equal(55)
  })
  it('#改进的递归法（数组保存已经计算过的值）', () => {
    expect(fib3(1)).to.be.equal(0)
    expect(fib3(2)).to.be.equal(1)
    expect(fib3(3)).to.be.equal(1)
    expect(fib3(4)).to.be.equal(2)
    expect(fib3(5)).to.be.equal(3)
    expect(fib3(10)).to.be.equal(34)
    expect(fib3(11)).to.be.equal(55)
  })
  it('#以2为底的对数',() => {
    expect(lg(4)).to.be.equal(Math.log2(4) | 0)
    expect(lg(5)).to.be.equal(Math.log2(5) | 0)
    expect(lg(6)).to.be.equal(Math.log2(6) | 0)
    expect(lg(7)).to.be.equal(Math.log2(7) | 0)
    expect(lg(8)).to.be.equal(Math.log2(8) | 0)
    expect(lg(9)).to.be.equal(Math.log2(9) | 0)
  })
  it('#sum',() => {
    expect(sum([0,1,2,3])).to.be.equal(6)
  })
  it('#histogram',() => {
    var arr1 = [2,2,2,3,3,4,5]
    var arr2 = histogram(arr1,8)
    expect(sum(arr2)).to.be.equal(arr1.length)
    expect(arr2).to.be.eql([0,0,3,2,1,1,0,0])
    expect(arr2.length).to.be.equal(8)
  })
  it('#mul',() => {
    for (let i = 0;i < 100;i++) {
      var a = sample(1,100)
      var b = sample(1,100)
      expect(mul(a,b)).to.be.equal(a * b)
    }
  })
  it('#pow',() => {
    for (let i = 0;i < 100;i++) {
      var a = sample(1,100)
      var b = sample(1,10)
      expect(pow(a,b)).to.be.equal(Math.pow(a,b))
    }
  })
  it('#factorial',() => {
    expect(factorial(5)).to.be.equal(120)
    expect(factorial(0)).to.be.equal(1)
    expect(factorial(1)).to.be.equal(1)
    expect(factorial(2)).to.be.equal(2)
    expect(factorial(10)).to.be.equal(3628800)
    expect(factorial(3)).to.be.equal(6)
  })
  it('#lnFactorial',() => {
    var diff = 1e-9;
    expect(Math.abs(lnFactorial(5) - Math.log(120))).below(diff)
    expect(Math.abs(lnFactorial(0) - Math.log(1))).below(diff)
    expect(Math.abs(lnFactorial(1) - Math.log(1))).below(diff)
    expect(Math.abs(lnFactorial(2) - Math.log(2))).below(diff)
    expect(Math.abs(lnFactorial(10) - Math.log(3628800))).below(diff)
    expect(Math.abs(lnFactorial(3) - Math.log(6))).below(diff)
  })
  it('#randomString',() => {
    console.log(randomString())
    console.log(randomString(1))
    console.log(randomString(5))
  })
  it('#gcd',() => {
    expect(gcd(1071, 462)).to.be.equal(21)
    expect(gcd(1111111, 1234567)).to.be.equal(1)
  })
  it('#gcd2',() => {
    expect(gcd2(1071, 462)).to.be.equal(21)
    expect(gcd2(1111111, 1234567)).to.be.equal(1)
  })
  it('#sortAsc',() => {
    expect(sortAsc(1071, 462, -6)).to.be.eql([-6,462,1071])
    expect(sortAsc(462, -6,1071)).to.be.eql([-6,462,1071])
    expect(sortAsc(1071, -6,462)).to.be.eql([-6,462,1071])
  })
  it('#binomial',() => {
    // 一个多小时没算出来，我也是醉了
    // var num = binomial(100,50,.25);
    // console.log(num,util.counter.binomial);
  })
  it('#betterBinomial',() => {
    // 100次伯努利实验，每次实验为1的概率为0.25,实验为0的概率为1-0.25 = 0.75
    // 出现50次1和50次0的概率是4.507310875086383e-8，经过7751次计算
    // var num = betterBinomial(100,50,.25);
    // console.log(num,util.counter.betterBinomial); // 4.507310875086383e-8 7751
    // var num = betterBinomial(100,50,.5);
    // console.log(num,util.counter.betterBinomial);
  })
  it('#unique',() => {
    expect(unique([1,1,23,343,32324,324,1])).to.be.eql([1,23,343,32324,324])
    expect(unique([1,23,343,32324,324])).to.be.eql([1,23,343,32324,324])
  })
  it('#rank',() => {
    var arr = [1,1,23,343,32324,324,1].sort((a,b) => a > b)
    expect(rank(343,arr)).to.be.eql(5)
  })
  it('#count',() => {
    expect(count(343,[1,1,23,343,32324,324,1].sort((a,b) => a > b))).to.be.eql(1)
  })
})