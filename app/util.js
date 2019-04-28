'use strict';

/**
 * 算法练习，参考：https://github.com/jimmysuncpt/Algorithms.git
 */

const ListNode = require('./Node');

module.exports = {
  counter: {
    binomial: 0,
    betterBinomial: 0
  },
  _readline: require('readline'),
  toBinaryString: function (number) {
    var s = '';
    if (number === 0) s = '0';
    for (var i = number; i > 0; i >>= 1) {
      s = (i % 2) + s;
    }
    return s;
  },
  fib: function (endNumber) {
    var f = 0,
      g = 1;
    for (var i = 0; i <= endNumber - 2; i++) {
      f = f + g;
      g = f - g;
    }
    return f;
  },
  fib2: function (endNumber) {
    if (endNumber === 1) return 0;
    if (endNumber === 2) return 1;
    return module.exports.fib2(endNumber - 2) + module.exports.fib2(endNumber - 1);
  },
  /**
   * 递归求斐波那契数列，缓存已经计算的结果
   */
  fib3: function (endNumber) {

    var fib = function (num, arr) {
      var e = arr[num - 1];
      if (!e) {
        // if (num == 1) arr[0] = 0;
        // else if (num == 2) arr[1] = 1;
        if (num < 3) arr[num - 1] = num - 1;
        else arr[num - 1] = fib(num - 1, arr) + fib(num - 2, arr);
      }
      return arr[num - 1];
    };

    var fibs = Array.from({ length: endNumber }, _ => 0);
    return fib(endNumber, fibs);
  },
  /**
   * ln(number!)
   * ln(a * b) = ln(a) + ln(b)，参见：https://zh.wikipedia.org/wiki/%E5%AF%B9%E6%95%B0
   */
  lnFactorial: function (number) {
    return number < 2 ? 0 : Math.log(number) + module.exports.lnFactorial(number - 1);
  },
  factorial: function (number) {
    var result = 1;
    while (number--) result *= (number + 1);
    return result;
  },
  /**
   * 以2为底的对数
   */
  lg: function (number) {
    var i, n;
    for (i = 0, n = 1; n <= number; i++)
      n <<= 1;
    return i - 1;
  },
  /**
   * @param  {[type]} arr    int数组
   * @param  {[type]} number int
   * @return 长度为number的数组，其中第i个元素的值为整数i在参数数组中出现的次数
   * 如果arr中的值均在[0,number)，返回数组中的所有元素之和应该和arr.length相等
   */
  histogram: function (arr, number) {
    var result = Array.from({ length: number }, _ => 0);
    for (let n of arr) n >= 0 && n < number && result[n]++;
    return result;
  },
  sum: function (arr) {
    return arr.reduce((a, b) => a + b);
  },
  /**
   * mul(7,8) => mul(14,4) => mul(28,2) => mul(56,1) => mul(128,0) + 56 => 56
   */
  mul: function (a, b) {
    if (b == 0) return 0;
    if (b % 2 == 0) return module.exports.mul(a + a, b / 2 | 0);
    return module.exports.mul(a + a, b / 2 | 0) + a;
  },
  /**
   * pow(2,3) => pow(4,1) * 2 => pow(16,0) * 4 * 2 => 8
   */
  pow: function (base, exponent) {
    if (exponent == 0) return 1;
    if (exponent % 2 == 0) return module.exports.pow(base * base, exponent / 2 | 0);
    return module.exports.pow(base * base, exponent / 2 | 0) * base;
  },
  sample: function (min, max) {
    return (Math.random() * (max - min + 1) + min) | 0;
  },
  randomString: function (length) {
    var tpl = Math.random().toString(36).slice(2);
    return length ? tpl.slice(0, length) : tpl;
  },
  shuffle: function (arr) {
    var len = arr.length;
    for (let i = 0; i < len; i++) {
      // 将arr[i]和与之后的元素随机交换
      var index = i + module.exports.sample(0, len - i - 1);
      require('./swap')(arr, i, index)
    }
    return arr
  },
  // 糟糕的洗牌算法，每个元素与数组中的任意一个值交换,上一个算法效率的1/6
  worstShuffle: function (arr) {
    var len = arr.length;
    for (let i = 0; i < len; i++) {
      var index = module.exports.sample(0, len - 1);
      require('./swap')(arr, i, index)
    }
    return arr
  },
  /**
   * https://zh.wikipedia.org/wiki/%E8%BC%BE%E8%BD%89%E7%9B%B8%E9%99%A4%E6%B3%95
   * 辗转相除法
   */
  gcd: function (p, q) {
    if (p < q) {
      var t = p;
      p = q;
      q = t;
    }
    // console.log(`p = ${p},q = ${q}`)
    return q == 0 ? p : module.exports.gcd(q, p % q);
  },
  /**
   * 更相减损法
   * https://zh.wikipedia.org/wiki/%E8%BC%BE%E8%BD%89%E7%9B%B8%E9%99%A4%E6%B3%95#/media/File:Euclidean_algorithm_252_105_animation_flipped.gif
   */
  gcd2: function (p, q) {
    if (p < q) {
      var t = p;
      p = q;
      q = t;
    }
    if (p == 0) return q;
    while (q) {
      if (p > q) p = p - q;
      else q = q - p;
    }
    return p;
  },
  /**
   * 3个数升序排列
   */
  sortAsc: function (a, b, c) {
    var t;
    if (a > b) t = a, a = b, b = t;
    if (a > c) t = c, c = a, a = t;
    if (b > c) t = b, b = c, c = t;
    return [a, b, c];
  },
  /**
   * 二项分布
   */
  binomial: function (n, k, p) {
    module.exports.counter.binomial++;
    if (n == 0 && k == 0) return 1;
    if (n < 0 || k < 0) return 0;
    return (1 - p) * module.exports.binomial(n - 1, k, p) + p * module.exports.binomial(n - 1, k - 1, p);
  },
  /**
   * http://blog.csdn.net/shuimu12345678/article/details/30773929
   */
  betterBinomial: function (n, k, p) {

    var bin = function (n, k, p) {
      module.exports.counter.betterBinomial++;
      if (n == 0 && k == 0) return 1;
      if (n < 0 || k < 0) return 0;
      if (matrix[n][k] == -1) {
        matrix[n][k] = (1 - p) * bin(n - 1, k, p) + p * bin(n - 1, k - 1, p);
      }
      return matrix[n][k];
    }

    // 初始化二维矩阵全-1
    var matrix = new Array(n + 1);
    for (let i = 0; i < n + 1; i++) {
      matrix[i] = Array.from({ length: k + 1 }, _ => -1);
    }

    return bin(n, k, p)
  },
  readLines: function (inputStream, callback) {
    var opts = {
      input: inputStream
    }
    var rl = module.exports._readline.createInterface(opts)
    var data = [];
    rl.on('line', function (line) {
      if (line) {
        line = line.trim()
        data.push(line)
      }
    }).on('close', function () {
      callback(data)
    })
  },
  readInts: function (inputStream, callback) {
    module.exports.readLines(inputStream, function (data) {
      callback(data.map(d => Number(d)).filter(d => !!d))
    })
  },
  binarySearch: require('./binarySearch').binarySearch,
  unique: function (arr) {
    var ret = [arr[0]];
    for (let i = 1; i < arr.length; i++) {
      if (module.exports.binarySearch(ret, arr[i]) === -1) ret.push(arr[i]);
    }
    return ret;
  },
  /**
   * @param  {[type]} key       键
   * @param  {[type]} sortedArr 有序数组，可能存在重复
   * @return 数组中小于该键的元素的数量
   */
  rank: function (key, sortedArr) {
    var index = module.exports.binarySearch(sortedArr, key);
    return index === -1 ? 0 : index;
  },
  /**
   * @param  {[type]} key       键
   * @param  {[type]} sortedArr 有序数组，可能存在重复
   * @return 数组中等于该键的元素的数量
   */
  count: function (key, sortedArr) {
    var count = 0;
    for (let i = 0; i < sortedArr.length; i++) {
      if (sortedArr[i] > key) break;
      if (sortedArr[i] === key) count++;
    }
    return count;
  },
  isPrime: function (num) {
    for (let i = 0; i < num / 2 | 0; i++) {
      if (num % i === 0) return false
    }
    return true
  },
  /**
   * 判断两个字符串是否是回环变位，字符串str1中的字符循环移动后可以得到str2
   * 例如：ACTGACC和TGACGAC
   */
  isCircularRotation: function (str1, str2) {
    return str1.length === str2.length && str1.repeat(2).indexOf(str2) != -1
  },
  toBinaryString: function (num) {
    var stack = []
    while (num) {
      stack.push(num % 2)
      num = num / 2 | 0
    }
    var str = ''
    // while(stack.length) {
    //     str += stack.pop()
    // }
    str = stack.reduceRight((prev, current) => '' + prev + current)
    return str
  },
  print(str) {
    process.stdout.write(str + '');
  },
  println(str) {
    console.log(str || '');
  },
  initMatrix(rowCount, colCount, defaultValue = 0) {
    const arr = new Array(rowCount);
    for (let i = 0; i < rowCount; i++) {
      arr[i] = new Array(colCount).fill(defaultValue);
    }
    return arr;
  },
  printMatrix(matrix) {
    for (let i = 0; i < matrix.length; i++) {
      for (let item of matrix[i]) {
        module.exports.print(item + '\t');
      }
      module.exports.println();
    }
  },
  readInts(path) {
    const rl = require('readlines');
    const lines = rl.readlinesSync(path).filter(x => x.length > 0);
    const arr = [];
    for (const line of lines) {
      const ints = line.split(/\s+/).map(x => parseInt(x)).filter(x => Number.isInteger(x));
      for (const int of ints) {
        arr.push(int);
      }
    }
    return arr;
  },
  createLinkedList(arr) {
    const len = arr.length;
    if (len === 0) return null;
    var head = new ListNode(arr[0]);
    var current = head;
    for (let i = 1; i < len; i++) {
      current.next = new ListNode(arr[i]);
      current = current.next; // 将current指针指向刚刚创建的新的节点
    }
    return head;
  },
  printLinkedList(head) {
    var current = head;
    var text = '';
    while (current != null) {
      text += current.value + ' -> ';
      current = current.next;
    }
    text += 'NULL';
    console.log(text);
  },
  findNode(head, value) {
    let cur = head;
    while (cur) {
      if (cur.value == value) return cur;
      cur = cur.next;
    }
    return null;
  },
  randomArray(min, max, len) {
    const _ = require('lodash');
    const arr = [];
    for (let i = 0; i < len; i++) {
      arr.push(_.random(min, max));
    }
    return arr;
  },
  randomDoubleArray(len) {
    const arr = [];
    while (len--) {
      arr.push(Math.random());
    }
    return arr;
  },
  sortedArray(len) {
    const arr = [];
    for (let i = 0; i < len; i++) {
      arr.push(i);
    }
    return arr;
  },
  sortedDescArray(len) {
    const arr = [];
    while (len--) {
      arr.push(len);
    }
    return arr;
  },
  twoElementsArray(len) {
    const arr = [];
    while (len--) {
      arr.push(Math.random() > .5 ? 1 : 0);
    }
    return arr;
  },
  sameArray(len) {
    return new Array(len).fill(0);
  },
  isSorted(arr) {
    for (let i = 1; i < arr.length; i++) {
      if (arr[i] < arr[i - 1]) {
        return false;
      }
    }
    return true;
  }
};