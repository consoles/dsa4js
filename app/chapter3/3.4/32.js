function hashCode(str) {
  let hash = 0;
  for (let i = 0; i < str.length; i++) {
    hash = hash * 31 + str.charCodeAt(i);
  }
  return hash;
}

// 寻找hash冲突

const hash1 = hashCode('Aa'); // 65 * 31 + 97
const hash2 = hashCode('BB'); // 66 * 31 + 66

// 某一位上的code-1，后一位的code+31，其他位置不变

// 可以推测出
// CC = Bb
// BBBB = BBAa

console.log(hash1, hash2);

function isAscii(charCode) {
  return charCode >= 65 && charCode <= 90 || charCode >= 97 && charCode <= 122;
}

function* buildStr(len) {

  const list = [];
  for (let i = 65; i <= 90; i++) {
    list.push(String.fromCharCode(i));
  }
  for (let i = 97; i <= 122; i++) {
    list.push(String.fromCodePoint(i));
  }

  const chars = new Array(len);

  function* _buildStr(chars, pos) {
    if (pos >= len) {
      return yield chars.join('');
    }
    for (let i = 0; i < list.length; i++) {
      chars[pos] = list[i];
      yield* _buildStr(chars, pos + 1);
    }
  }

  return yield* _buildStr(chars, 0);
}

function buildHashCode(n) {

  const len = 2 ** n;

  let results = [];

  const map = {};

  let maxHash = 0;

  for (const str of buildStr(len)) {
    const hash = hashCode(str);
    map[hash] = map[hash] || new Set();
    const charList = str.split('');
    const ret = [];
    _buildHashCode(charList, 0, ret);
    for (const item of ret) {
      map[hash].add(item);
    }
    if (map[hash].size >= len) {
      maxHash = hash;
      break;
    }
  }

  // 没有找到指定长度的，返回所有字符串中hash冲突最多的那些字符串
  if (maxHash === 0) {
    for (const hash of Object.keys(map)) {
      maxHash = Math.max(maxHash,map[hash].length);
    }
  }

  return [...map[maxHash]];

  function _buildHashCode(charList, pos, ret) {
    if (pos + 1 >= charList.length) {
      return;
    }
    let cur = charList[pos];
    let next = charList[pos + 1];

    let charCodeCur = cur.charCodeAt(0) - 1;
    let charCodeNext = next.charCodeAt(0) + 31;

    if (isAscii(charCodeCur) && isAscii(charCodeNext)) {
      charList[pos] = String.fromCodePoint(charCodeCur);
      charList[pos + 1] = String.fromCodePoint(charCodeNext);
      ret.push(charList.join(''));
    }
    _buildHashCode(charList, pos + 1, ret);
    charList[pos] = cur;
    charList[pos + 1] = next;
  }
}

const ret = buildHashCode(2);
for (const str of ret) {
  console.log(str, hashCode(str));
}
