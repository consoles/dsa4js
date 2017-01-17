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
var mystery = (a,b) => {
  if (b == 0) return 0;
  if (b % 2 == 0) return mystery(a + a,b / 2 | 0);
  return mystery(a + a,b / 2 | 0) + a;
}

var mystery2 = (a,b) => {
  if (b == 0) return 1;
  if (b % 2 == 0) return mystery2(a * a,b / 2 | 0);
  return mystery2(a * a,b / 2 | 0) * a;
}

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
var genData = function() {

  var randomString = util.randomString
  var sample = util.sample
  var records = [];
  for (let i = 0;i < 6;i++) {
    let name = randomString(5);
    let score1 = sample(0,100)
    let score2 = sample(0,100)
    let record = `${name}|${score1}|${score2}`
    records.push(record)
  }
  // console.log(records.join(require('os').EOL))
  var content = records.join(require('os').EOL)
  fs.writeFile('./input/scores.txt',content)
}
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

var count = util.rank(5,arr)
// console.log(count)

// 计算2各骰子之和的概率分布,当做1000000次随机试验的时候误差在1/1000
const SIDES = 6;

var dist = Array.from({length: 2 * SIDES + 1},_ => 0);

for(let i = 1;i <= SIDES; i++)
  for(let j = 1;j <= SIDES;j++)
    dist[i+j] += 1;

var p = {};
var sum = 0;
for(let i = 2;i <= 2 * SIDES;i++)
  p[i] = dist[i] / 36,sum += p[i];

// console.log(p,sum)

// 模拟n次同时投掷2个骰子
var exp = function(num){
  var result = []
  for(let i = 0;i < num;i++){ 
    let sum =  util.sample(1,6) + util.sample(1,6)
    result.push(sum)
  }
  return result
}

var testCount = 1000000;
var result = exp(testCount);
var counter = {};
for(let item of result)
  counter[item] = (counter[item] | 0) + 1;

var testP = {}
for(let count in counter){
  testP[count] = counter[count] / testCount
}
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
const T = 10;
var batch = function(N){

  for(let i = 0;i < T;i++) {
    var arr1 = [],arr2 = [];
    for(let j = 0;j < N;j++) {
      arr1.push(util.sample(100000,999999))
      arr2.push(util.sample(100000,999999))
    }

    sort(arr1)
    sort(arr2)
    let count = 0 
    for(let k = 0;k < N;k++){
      let value = arr1[k];
      if (binarySearch(arr2,value) > -1) count++  
    }
    console.log(`TEST-${i},N = ${N},count=${count},percent=${count / N}`)
  }
}
batch(10e3)
batch(10e4) 
batch(10e5) 
batch(10e6)