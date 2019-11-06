// key1 => [a,b,c]
// key2 => [a,e,f]

// 转化为
// a => [key1,key2]
// b => [key1]
// c => [key1]

function invert(data) {
  const map = new Map();
  for (const key of Object.keys(data)) {
    const value = data[key];
    for (const v of value) {
      if (!map.has(v)) {
        map.set(v, []);
      }
      map.get(v).push(key);
    }
  }
  return map;
}

const data = {
  key1: 'abc'.split(''),
  key2: 'aef'.split('')
};

const ret = invert(data);
debugger;
