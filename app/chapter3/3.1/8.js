const path = require('path');

const WordCounter = require('../WordCounter');
const {readLinesAsync} = require('../../util');

const tale = path.join(__dirname, '../../../test/input/algs4-data/tale.txt');

(async () => {
  const lines = await readLinesAsync(tale);
  const counter = new WordCounter();
  for (const line of lines) {
    const words = line.split(/\s+/).map(x => x.trim()).filter(x => x.length > 0);
    for (const word of words) {
      if (word.length >= 10) {
        if (!counter.contains(word)) {
          counter.put(word, 1);
        } else {
          counter.put(word, counter.get(word) + 1);
        }
      }
    }
  }
  let maxKey = '';
  counter.put(maxKey, 0);
  for (const key of counter.keys()) {
    if (counter.get(key) > counter.get(maxKey)) {
      maxKey = key;
    }
  }
  console.log(maxKey, counter.get(maxKey));
})();

// monseigneur 101
