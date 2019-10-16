function hash(a, M, char, k) {
  return (a * k) % M;
}

const items = 'SEARCHXMPL'.split('');

let run = true;
for (let M = 2; M <= items.length && run; M++) {
  for (let a = 1; a < 100000; a++) {
    const s = new Set();
    for (let i = 0; i < items.length; i++) {
      let h = hash(a, M, items[i], i);
      s.add(h);
    }
    console.log('a = ', a, 'M = ', M, s.size);
    if (s.size === items.length) {
      console.log('网格搜索得到完美散列函数', 'a = ', a, 'M = ', M);
      run = false;
      break;
    }
  }
}
