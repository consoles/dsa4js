const path = require('path');

const fs = require('fs');

const {readLinesAsync} = require('../../util');

const tale = path.join(__dirname, '../../../test/input/algs4-data/tale.txt');

function wordCount(arr) {

  // 基于链表的符号表
  // 蓝色折线图(x,y) x:get&put的操作次数的和，y:总运行时间

  // 基于平行数组的符号表
  // 绿色折线图

  const sts = [require('../SequentialSearchST'), require('../BinarySearchST'), require('../BST')].map(ClZ => new ClZ());

  const data = [];

  for (const st of sts) {
    const type = st.constructor.name;
    let x = 0;
    const start = Date.now();

    for (const word of arr) {
      if (st.contains(word)) {
        st.put(word, st.get(word) + 1);
        x += 2;
      } else {
        st.put(word, 1);
        x++;
      }
      data.push({x, y: Date.now() - start, type});
    }
    let maxKey = '';
    st.put(maxKey, 0);
    for (const key of st.keys()) {
      if (st.get(key) > st.get(maxKey)) {
        maxKey = key;
      }
      x += 2;
      data.push({x, y: Date.now() - start, type});
    }
  }

  fs.writeFileSync(path.join(__dirname, '45.g2.data.json'), JSON.stringify(data));
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

  wordCount(words);
})();
