function frequency(words) {
  const counter = {};
  const wordsUniq = [];
  for (const word of words) {
    counter[word] = counter[word] || 0;
    const count = counter[word]++;
    if (count === 0) {
      wordsUniq.push(word);
    }
  }
  wordsUniq.sort((a, b) => counter[b] - counter[a]);
  for (const word of wordsUniq) {
    console.log(word, '->', counter[word]);
  }
}

const words = ['a', 'n', 'b', 'a', 'c', 'd', 'n', 's', 'a'];
frequency(words);
