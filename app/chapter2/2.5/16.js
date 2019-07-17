function californiaCompare(str1, str2) {
  const order = 'RWQOJMVAHBSGZXNTCIEKUPDYFL';
  const n = Math.min(str1.length, str2.length);
  for (let i = 0; i < n; i++) {
    const a = order.indexOf(str1[i]);
    const b = order.indexOf(str2[i]);
    if (a !== b) return a - b;
  }
  return str1.length - str2.length;
}

(async () => {
  const axios = require('axios');
  const {data} = await axios.get('https://introcs.cs.princeton.edu/java/data/california-gov.txt');
  const names = data.split('\n').map(x => x.trim().toUpperCase()).filter(x => x.length > 0);
  names.sort((name1, name2) => californiaCompare(name1, name2));
  console.log(names);
})();
