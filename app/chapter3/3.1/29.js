const BinarySearchST = require('../BinarySearchST');

function testBinarySearchST() {
  const keys = "S E A R C H E X A M P L E".split(/\s+/);
  const n = keys.length;

  const st = new BinarySearchST();

  keys.forEach((value, index) => st.put(value, index));

  console.log('size = ', st.size);
  console.log('minKey = ', st.minKey);
  console.log('maxKey = ', st.maxKey);
  console.log();

  console.log('test keys:');
  for (const key of st.keys) {
    console.log(key);
  }
  console.log();

  console.log('test select(应该和keys的顺序一样):');
  for (let i = 0; i < st.size; i++) {
    console.log(i, st.select(i));
  }
  console.log();

  console.log('rank & floor & ceil');
  for (let i = 'A'.charCodeAt(0); i <= 'Z'.charCodeAt(0); i++) {
    const c = String.fromCodePoint(i);
    console.log(`c = ${c},rank = ${st.rank(c)},floor = ${st.floor(c)},ceil = ${st.ceil(c)}`);
  }
  console.log();

  for (let i = 0; i < st.size / 2; i++) {
    st.deleteMin();
  }
  console.log('after delete 1/2 min keys');
  for (const key of st.keys) {
    console.log(key, st.get(key));
  }
  console.log();

  while (!st.isEmpty()) {
    st.delete(st.select(parseInt(st.size / 2)));
  }
  console.log('after delete all remain keys');
  for (const key of st.keys) {
    console.log(key, st.get(key));
  }
  console.log();

  console.log('after adding back n keys');
  keys.forEach((value, index) => st.put(value, index));
  for (const key of st.keys) {
    console.log(key, st.get(key));
  }
  console.log();
}

testBinarySearchST();
