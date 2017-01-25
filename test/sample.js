'use strict';

/**
 * some idea to try
 *
 * 《算法》 4 edtion
 */

// 1.1.16
// var exR1 = n => n <= 0 ? '' : exR1(n - 3) + n + exR1(n - 2) + n; 
// console.log(exR1(6)) // 311361142246

// 1.1.17
// var exR2 = n => {
//   var str = exR2(n - 3) + n + exR2(n - 2) + n; 
//   if (n <= 0) return '';
//   return str;
// };

// console.log(exR2(6)); // RangeError: Maximum call stack size exceeded

// 1.1.18
// var mystery = (a,b) => {
//   if (b == 0) return 0;
//   if (b % 2 == 0) return mystery(a + a,b / 2 | 0);
//   return mystery(a + a,b / 2 | 0) + a;
// }

// var mystery2 = (a,b) => {
//   if (b == 0) return 1;
//   if (b % 2 == 0) return mystery2(a * a,b / 2 | 0);
//   return mystery2(a * a,b / 2 | 0) * a;
// }

// console.log(mystery(2,25)) // 50
// console.log(mystery(3,11)) // 33
// console.log(mystery(7,8)) // 56

// console.log(mystery2(2,25)) // 33554432
// console.log(mystery2(3,11)) // 177147
// console.log(mystery2(2,8)) // 256
// console.log(mystery2(3,3)) // 27

var fs = require('fs')
var util = require('../app/util')
// 1.1.20
// var genData = function() {

//   var randomString = util.randomString
//   var sample = util.sample
//   var records = [];
//   for (let i = 0;i < 6;i++) {
//     let name = randomString(5);
//     let score1 = sample(0,100)
//     let score2 = sample(0,100)
//     let record = `${name}|${score1}|${score2}`
//     records.push(record)
//   }
//   // console.log(records.join(require('os').EOL))
//   var content = records.join(require('os').EOL)
//   fs.writeFile('./input/scores.txt',content)
// }
// genData()

// var readline = require('readline')
// const opts = {
//     input: fs.createReadStream('./input/scores.txt'),
//     output: process.stdout,
//     terminal: false
// }
// var rl = readline.createInterface(opts)
// rl.on('line',function(line) {
//   var values = line.split('|')
//   var name = values[0]
//   var score1 = values[1]
//   var score2 = values[2]
//   var rate = (Number(score1) + Number(score2)) / 2
//   console.log(name,score1,score2,rate)
// })

var printDepth = function(depth) {
  var char = '-'
  while(depth--) process.stdout.write(char)
}

var binarySearchByRecursion = (arr, element, start, end,depth) => {

  printDepth(depth)
  console.log(`start:${start},end:${end}`)
  if (start <= end) {
    let mid = (start + end) >> 1
    if (arr[mid] === element)
      return mid
    if (element > arr[mid])
      return binarySearchByRecursion(arr, element, mid + 1, end,depth + 1)
    return binarySearchByRecursion(arr, element, start, end - 1,depth + 1)
  }
  return -1
}

var arr = [1,2,3,4,5,6,7,8]
var start = 0,end = arr.length
// console.log(binarySearchByRecursion(arr,6,start,end,0))

// util.readLines(fs.createReadStream('./util.test.js'),function(data){
//   console.log(data)
// })

const sort = require('../app/sort.js').mergeSort
const binarySearch = require('../app/binarySearch.js').binarySearch

// util.readInts(fs.createReadStream('./input/largeW.txt'),function(data) {
//   var writeList = sort(data);
//   util.readInts(fs.createReadStream('./input/largeT.txt'), function(data) {
//     data.forEach(function(d){
//       if (binarySearch(writeList,d) < 0) {
//         console.log(d)
//       }
//     })
//   })
// })

// var count = util.rank(5,arr)
// console.log(count)

// 计算2各骰子之和的概率分布,当做1000000次随机试验的时候误差在1/1000
// const SIDES = 6;

// var dist = Array.from({length: 2 * SIDES + 1},_ => 0);

// for(let i = 1;i <= SIDES; i++)
//   for(let j = 1;j <= SIDES;j++)
//     dist[i+j] += 1;

// var p = {};
// var sum = 0;
// for(let i = 2;i <= 2 * SIDES;i++)
//   p[i] = dist[i] / 36,sum += p[i];

// console.log(p,sum)

// 模拟n次同时投掷2个骰子
// var exp = function(num){
//   var result = []
//   for(let i = 0;i < num;i++){ 
//     let sum =  util.sample(1,6) + util.sample(1,6)
//     result.push(sum)
//   }
//   return result
// }

// var testCount = 1000000;
// var result = exp(testCount);
// var counter = {};
// for(let item of result)
//   counter[item] = (counter[item] | 0) + 1;

// var testP = {}
// for(let count in counter){
//   testP[count] = counter[count] / testCount
// }
// console.dir(testP)

// 1.1.36 乱序检查
/**
 * 构造一个大小为num的数组
 */
// var genArr = function(num) {
//   var result = []
//   for(let i = 0;i < num;i++) result.push(i)
//   return result  
// }

// const M = 8,N = 10;
// result = [];
// for(let i = 0;i < N;i++){
//   let ret = util.shuffle(genArr(M));
//     ret = util.worstShuffle(genArr(M));
//   result.push(ret);
// }
// console.dir(result)
// counter = []
// for(let i = 0;i < M;i++) counter[i] = Array.from({length: M}, _ => 0);

// for(let i = 0;i < result.length;i++){
//   for(let j = 0;j < result[i].length;j++){
//     let entry = result[i][j]
//     counter[entry][j]++
//   }
// }
// console.dir(counter)

// 1.1.39
// const T = 10;
// var batch = function(N){

//   var sum = 0
//   for(let i = 0;i < T;i++) {
//     var arr1 = [],arr2 = [];
//     for(let j = 0;j < N;j++) {
//       arr1.push(util.sample(100000,999999))
//       arr2.push(util.sample(100000,999999))
//     }

//     sort(arr1)
//     sort(arr2)
//     let count = 0 
//     for(let k = 0;k < N;k++){
//       let value = arr1[k];
//       if (binarySearch(arr2,value) > -1) {
//         count++
//         sum++
//       }
//     }
//     console.log(`TEST-${i},N = ${N},count=${count},percent=${count / N}`)
//   }
//   console.log(`N = ${N},avg = ${sum / N}`)
// }
// batch(10e3)
// batch(10e4) 
// batch(10e5) 
// batch(10e6)

// 1.2.12 日期类型的2种实现
// class Date{
//   constructor(year,month,day){
//     this.year = year
//     this.month = month
//     this.day = day
//   }
//   toString(){
//     return [this.year,this.month,this.day].join('-')
//   }
// }
// class Date2 {
//   /**
//    * 这种实现的正确性基于日的范围位于0~31，月的值位于0~15，年的值为正
//    * 512 / 32 = 16
//    */
//   constructor(year,month,day){
//     this.value = year * 512 + month * 32 + day
//   }
//   get year(){
//     return this.value / 512 | 0
//   }
//   get month(){
//     return (this.value / 32 | 0) % 16
//   }
//   get day(){
//     return this.value % 32
//   }
//   toString(){
//     return [this.year,this.month,this.day].join('-')
//   }
// }

// var d = new Date(2016,12,28)
// console.log(d.toString())
// d = new Date2(2016,12,28)
// console.log(d.toString())

// 1.2.13 累加器
// class Accumulator{
//   constructor(){
//     this.total = 0
//     this.count = 0
//   }
//   addDataValue(value){
//     this.total += value
//     this.count++
//   }
//   mean(){
//     return this.total / this.count
//   }
//   toString(){
//     return `MEAN (${this.count}) values:${this.mean()}`
//   }
// }

// var a = new Accumulator()
// for(let i = 0;i < 10000000;i++) a.addDataValue(Math.random())
// console.log(a.toString()) // 试验次数越多，值越接近0.5

// 1.2.7
// var mystery = function(str){
//   var N = str.length
//   if (N <= 1) return str

//   var mid = N / 2 | 0
//   var a = str.slice(0,mid)
//   var b = str.slice(mid,N)
//   return mystery(b) + mystery(a) 
// }

// var ret = mystery('abc')
// console.log(ret)
// ret = mystery('abcd')
// console.log(ret)

// 1.3.9 编写一段程序，从标准输入得到一个缺少左括号的表达式并打印出补全括号之后的中序表达式。例如，给定输入：
// 1 + 2 ) * 3 - 4 ) * 5 - 6 ) ) ) 输出结果为( ( 1 + 2 ) * ( ( 3 - 4 ) * ( 5 - 6 ) ) )
// var getCompleteException = function(expStr) {
//   // 类似于迪杰斯特拉的双栈法求值，符号栈和值栈，遇到右括号的时候从值栈中弹出2个数字，从符号栈中弹出操作符，组成 (num1 op num 2)的形式放入新的值栈
//   // 这样的好处是可以灵活处理嵌套关系
//     var opStack = []
//     var valueStack = []
//     var tokens = expStr.split(/\s+/)
//     for(let token of tokens) {
//       if(token == '+' || token == '-' || token == '*' || token == '/') opStack.push(token)
//       else if(token == ')') {
//         var num2 = valueStack.pop()
//         var num1 = valueStack.pop()
//         var op = opStack.pop()
//         valueStack.push(`( ${num1} ${op} ${num2} )`) // new value
//       } else {
//         valueStack.push(token)
//       }
//     }
//     return valueStack.pop()
// }

// var ret = getCompleteException('1 + 2 ) * 3 - 4 ) * 5 - 6 ) ) )') // ( ( 1 + 2 ) * ( ( 3 - 4 ) * ( 5 - 6 ) ) )
// console.log(ret)

var ret = util.infixToPostfix('( ( 1 + 2 ) * ( ( 3 - 4 ) * ( 5 - 6 ) ) )')
console.log(ret)