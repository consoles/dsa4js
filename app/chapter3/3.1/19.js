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
      if (word.length >= 2) {
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

  const q = new Set();

  // 传统算法，遍历符号表找出出现单词最多的次数，再次遍历符号表，找出等于最多次数的单词
  // 这个算法利用队列的思想仅需要遍历一次符号表

  for (const key of counter.keys()) {
    const count = counter.get(key);
    const maxCount = counter.get(maxKey);
    if (count > maxCount) {
      q.clear();
      q.add(key);
      maxKey = key;
    } else if (count === maxCount) {
      q.add(key);
    }
  }

  console.log(q);

})();
