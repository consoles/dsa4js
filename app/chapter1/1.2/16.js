'use strict'

const assert = require('assert');

/**
 * 有理数都可以表示为分数
 */
class Rational {
  /**
   * numerator 分子
   * denominator 分母
   */
  constructor(numerator, denominator) {
    assert(Number.isInteger(numerator) && Number.isInteger(denominator), '分子，分母必须是整数')
    assert(denominator !== 0, '分母不能为0')

    this.isNegative = false
    if (numerator * denominator < 0) {
      this.isNegative = true
      numerator = Math.abs(numerator)
      denominator = Math.abs(denominator)
    }
    var g = require('../../util').gcd(numerator, denominator)
    this.numerator = numerator / g
    this.denominator = denominator / g
    if (this.isNegative) this.numerator = -this.numerator
  }

  plus(rational) {
    var newDenominator = this.denominator * rational.denominator,
      newNumerator = this.numerator * rational.denominator + rational.numerator * this.denominator
    return new Rational(newNumerator, newDenominator)
  }

  negate() {
    // deep clone
    var newRational = new Rational(this.numerator, this.denominator)
    newRational.numerator = -newRational.numerator
    newRational.isNegative = !newRational.isNegative
    return newRational
  }

  minus(rational) {
    return this.plus(rational.negate())
  }

  times(rational) {
    var newDenominator = this.denominator * rational.denominator
    var newNumerator = this.numerator * rational.numerator
    return new Rational(newNumerator, newDenominator)
  }

  divides(rational) {
    if (!rational.numerator) throw new TypeError('denominator is zero')
    var newRational = new Rational(rational.denominator, rational.numerator)
    return this.times(newRational)
  }

  equals(rational) {
    var newRational = new Rational(rational.numerator, rational.denominator)
    return newRational.numerator === this.numerator && newRational.denominator === this.denominator
  }
}

var r1 = new Rational(1, 2)
var r2 = new Rational(3, 2)
console.log('r1', r1, 'r2', r2)
console.log(r1.plus(r2)) // 2
console.log(r1.minus(r2)) // -1
console.log(r1.times(r2)) // 3/4
console.log(r1.negate()) // -1/2
console.log(r1.divides(r2)) // 1 / 3
console.log(r1.equals(r2)) // false
console.log(r1.equals(new Rational(3, 6))) // true
debugger