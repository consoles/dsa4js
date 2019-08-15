const path = require('path');

const {readLinesAsync} = require('../../util');

const tale = path.join(__dirname, '../../../test/input/algs4-data/tale.txt');

const SequentialSearchST = require('../SequentialSearchST');

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
    const time = wordCount(SequentialSearchST, words.slice(0, n));
    console.log('n = ', n, 'time = ', time);
    n *= 2;
  }
})();

// n =  1000 time =  32.3
// n =  2000 time =  97.8
// n =  4000 time =  281.6
// n =  8000 time =  917.3
// n =  16000 time =  3031.7
// n =  32000 time =  8707.9
// n =  64000 time =  31146.8
// n =  128000 time =  132635.7

// 由于包含重复单词，因此结果会比 4 略低一些。
