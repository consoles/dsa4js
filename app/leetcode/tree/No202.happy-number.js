/**
 * @param {number} n
 * @return {boolean}
 */
var isHappy = function (n) {
  const set = new Set();
  while (true) {
    if (set.has(n)) {
      break;
    }
    set.add(n);
    let sum = 0;
    while (n) {
      const num = n % 10;
      n = n / 10 | 0;
      sum += num ** 2;
    }
    if (sum === 1) {
      return true;
    }
    n = sum;
  }
  return false;
};

function* genNextSquareSum(n) {
  while(true) {
    let sum = 0;
    while(n !==0) {
      sum += (n % 10) ** 2
      n = n / 10 | 0
    }
    yield sum
    n = sum
  }
}

function isHappy2(n) {
  const set = new Set();
  for (const n of genNextSquareSum(n)) {
    if (n === 1) {
      return true
    }
    if (set.has(n)) {
      return false
    }
    set.add(n)
  }
  return false
}

function squareSum(n) {
  let sum = 0;
  while(n !== 0) {
    sum += (n % 10) ** 2
    n = n / 10 | 0
  }
  return sum
}

function isHappy3(n) {
  // 使用 set 保存已经出现过的平方和可能会导致循环特别长的问题。如果出现循环则是一个隐式的环形链表。
  // 可以用快慢指针来判断是否出现循环
  let fast = n
  let slow = n
  while(true) {
    fast = squareSum(fast)
    fast = squareSum(fast)
    slow = squareSum(slow)
    if (fast === slow) {
      return fast === 1
    }
  }
}

let ret  = isHappy(3);
console.log(ret)
