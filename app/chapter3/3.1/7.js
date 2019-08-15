const {randomArray} = require('../../util');

function uniqCount(arr) {
  return new Set(arr).size;
}

let n = 10;
const testCount = 10;
while (n <= 1e6) {
  let sum = 0;
  for (let i = 0; i < testCount; i++) {
    const arr = randomArray(0, 999, n);
    sum += uniqCount(arr);
  }
  console.log('n = ', n, 'count = ', parseInt(sum / testCount));
  n *= 10;
}

// n =  10 count =  10
// n =  100 count =  95
// n =  1000 count =  629
// n =  10000 count =  1000
// n =  100000 count =  1000
// n =  1000000 count =  1000
