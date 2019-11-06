// 页码对应单词
const dict = {
  1: ['hello', 'world', 'i', 'love', 'you'],
  2: ['book', 'hello', 'apple', 'love'],
  3: ['thanks', 'world', 'banana'],
  4: ['apple', 'sun', 'hello']
};

// 单词对应页码
const map = {};
for (const pageNo of Object.keys(dict)) {
  const words = dict[pageNo];
  for (const word of words) {
    map[word] = map[word] || [];
    map[word].push(pageNo);
  }
}

console.log(map);

console.log('还原原始字典');

const d = {};
for (const word of Object.keys(map)) {
  const pageNos = map[word];
  for (const pageNo of pageNos) {
    d[pageNo] = d[pageNo] || [];
    d[pageNo].push(word);
  }
}

console.log(d);
