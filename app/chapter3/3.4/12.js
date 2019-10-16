// 生成所有的输入顺序，然后针对每个插入顺序进行验证，居然没有！！！真尴尬

function allStrSeq(str) {
  const res = [];
  const end = str.length - 1;
  const arr = str.split('');

  function dfs(aux, i) {
    if (i === end) {
      res.push(aux.slice());
      return;
    }
    for (let j = i; j <= end; j++) {
      [aux[i], aux[j]] = [aux[j], aux[i]];
      dfs(aux, i + 1);
      [aux[i], aux[j]] = [aux[j], aux[i]];
    }
  }

  dfs(arr, 0);
  return res;
}

const LinearProbingHashST = require('../LinearProbingHashST');

class LinearProbingHashST12 extends LinearProbingHashST {
  constructor() {
    // 固定容量7
    super(7);
  }

  _resize() {
    // 不具备扩容功能
  }
}

class Char {
  constructor(key, index) {
    this.key = key;
    this.index = index;
  }

  hashCode() {
    const map = {
      A: 2,
      B: 0,
      C: 0,
      D: 4,
      E: 4,
      F: 4,
      G: 2
    };
    return map[this.key];
  }

  equals(other) {
    return this.key === other.key;
  }
}

const seq = allStrSeq('ABCDEFG');

const testArray = [
  'EFGACBD',
  'CEBGFDA',
  'BDFACEG',
  'CGBADEF',
  'FGBDACE',
  'GECADBF'
];

for (const items  of seq) {
  const st = new LinearProbingHashST12();

  for (let i = 0; i < items.length; i++) {
    st.put(new Char(items[i], i), i);
  }
  const str = st.keys.map(x => x.key).join('');
  if (testArray.indexOf(str) !== -1) {
    console.log(items.join(''), '->', str);
  }
}
