/**
 * O(N^3)
 * @param words
 * @returns {Array}
 */
function findComposeWords(words) {
  const ret = [];
  for (let i = 0; i < words.length; i++) {
    for (let j = 0; j < words.length; j++) {
      const word1 = words[i];
      const word2 = words[j];
      if (word1 !== word2) {
        const word = word1 + word2;

        for (const w of words) {
          if (w === word) {
            ret.push(w);
            break;
          }
        }
      }
    }
  }
  return ret;
}

function binarySearch(keys, len, lo, hi) {
  while (lo <= hi) {
    let mid = lo + parseInt((hi - lo) / 2);
    // 从左向右第一个长度为len的字符串所在的索引
    if (keys[mid].length === len) {
      while (mid - 1 >= lo && keys[mid - 1].length === len) {
        mid--;
      }
      return mid;
    }
    if (len > keys[mid].length) {
      lo = mid + 1;
    } else {
      hi = mid - 1;
    }
  }
  return -1;
}

function findComposeWords2(words) {
  const n = words.length;
  // 将字符串按照长度进行排序
  words.sort((a, b) => a.length - b.length);
  // 第一个字符串最短，则组合词的最短长度为第一个字符串的长度 * 2
  const minLength = 2 * words[0].length;
  let canComposeIndex = 0;
  while (canComposeIndex < n && words[canComposeIndex].length < minLength) {
    canComposeIndex++;
  }
  // 从canComposeIndex开始逐个检查是否是组合词
  // 如果words[canComposeIndex]是组合词，那么它一定是位于它之前的某两个字符串组合而成。组合词的长度一定等于被组合词的长度之和，因此我们可以通过长度快速判断有可能的组合词
  // 现在问题就转化成了two-sum问题
  while (canComposeIndex < n) {
    const len = words[canComposeIndex].length;
    const keyWord = words[canComposeIndex];
    for (let i = 0; i < canComposeIndex; i++) {
      // 二分查找确定i到canComposeIndex有没有长度为len的字符串
      let start = binarySearch(words, len - words[i].length, i, canComposeIndex);
      if (start !== -1) {
        while (words[start].length + words[i].length === len) {
          const composeWords1 = words[start] + words[i];
          const composeWords2 = words[i] + words[start];
          if (composeWords1 === keyWord) {
            console.log('compose words 1', composeWords1);
          } else if (composeWords2 === keyWord) {
            console.log('compose words 2', composeWords2);
          }
          start++;
        }
      }
    }
    canComposeIndex++;
  }
}

const words = ['after', 'thought', 'afterthought'];

const ret = findComposeWords2(words);
debugger;
