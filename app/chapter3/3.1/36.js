const path = require('path');

const {readLinesAsync} = require('../../util');

const tale = path.join(__dirname, '../../../test/input/algs4-data/tale.txt');

const BinarySearchST = require('../BinarySearchST');

function wordCount(ST, arr) {
  let sum = 0;
  let testCount = 10;

  for (let i = 0; i < testCount; i++) {
    const start = Date.now();
    const st = new ST();
    for (const key of arr) {
      if (st.contains(key)) {
        st.put(key, st.get(key) + 1);
      } else {
        st.put(key, 1);
      }
    }
    let maxKey = -1;
    st.put(maxKey, 0);
    for (const key of st.keys) {
      if (st.get(key) > st.get(maxKey)) {
        maxKey = key;
      }
    }
    sum += (Date.now() - start);
  }

  return sum / testCount;
}

(async () => {
  const words = []; // len 135643
  const lines = await readLinesAsync(tale);

  for (const line of lines) {
    const wordsInLine = line.split(/\s+/).map(x => x.trim()).filter(x => x.length > 0);
    for (const word of wordsInLine) {
      words.push(word);
    }
  }

  let n = 1000;
  while (n < words.length) {
    const time = wordCount(BinarySearchST, words.slice(0, n));
    console.log('n = ', n, 'time = ', time);
    n *= 2;
  }
})();

// n =  1000 time =  18.6
// n =  2000 time =  30.9
// n =  4000 time =  31.9
// n =  8000 time =  79.6
// n =  16000 time =  164.3
// n =  32000 time =  390
// n =  64000 time =  776.9
// n =  128000 time =  1775.9

// 基于二分查找的性能随着输入规模的增大，性能优势越来越明显
