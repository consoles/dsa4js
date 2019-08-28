const {randomArray} = require('../../util');
const BST = require('../BST');

function test(n) {

  let testCount = 100;

  let totalHeight = 0;

  for (let i = 0; i < testCount; i++) {
    const arr = randomArray(1, n, n);
    const bst = new BST();
    for (let i = 0; i < arr.length; i++) {
      bst.put(arr[i], i);
    }
    totalHeight += bst.height;
  }

  console.log('n = ', n, 'height', totalHeight / testCount, '理论值(2.99*lgN)', 2.99 * Math.log2(n));
}

for (let i = 1e4; i <= 1e6; i *= 10) {
  test(i);
}

// n =  10000 height 28.28 理论值(2.99*lgN) 39.73026001485285
// n =  100000 height 37.99 理论值(2.99*lgN) 49.662825018566075
// n =  1000000 height 46.92 理论值(2.99*lgN) 59.59539002227928
