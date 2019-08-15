const path = require('path');

const WordCounter = require('../WordCounter');
const {readLinesAsync} = require('../../util');

const tale = path.join(__dirname, '../../../test/input/algs4-data/tale.txt');

(async () => {
  const lines = await readLinesAsync(tale);

  for (let len of [1, 8, 10]) {
    const counter = new WordCounter();
    for (const line of lines) {
      const words = line.split(/\s+/).map(x => x.trim()).filter(x => x.length > 0);
      for (const word of words) {
        if (word.length >= len) {
          if (!counter.contains(word)) {
            counter.put(word, 1);
          } else {
            counter.put(word, counter.get(word) + 1);
          }
        }
      }
    }
    let maxKey = '';
    console.log('len = ', len, 'wordCount', counter.wordCount, 'lastPut', counter.lastPut);
    counter.put(maxKey, 0);
    for (const key of counter.keys()) {
      if (counter.get(key) > counter.get(maxKey)) {
        maxKey = key;
      }
    }
    console.log('len = ', len, maxKey, counter.get(maxKey));
  }
})();

// len =  1 wordCount 271286 lastPut known
// len =  1 the 7989
// len =  8 wordCount 28692 lastPut faltering
// len =  8 business 122
// len =  10 wordCount 9158 lastPut disfigurement
// len =  10 monseigneur 101
