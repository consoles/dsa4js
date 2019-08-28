// 官方实现：https://algs4.cs.princeton.edu/32bst/TestBST.java.html

const BST = require('../BST');

function testBST() {
  const bst = new BST();
  const keys = 'SEARCHEXAMPLE'.split('');
  for (let i = 0; i < keys.length; i++) {
    bst.put(keys[i], i);
  }

  console.log('size = ' + bst.size);
  console.log('min  = ' + bst.min);
  console.log('max  = ' + bst.max);
  console.log();

  // print keys in order using allKeys()
  console.log('Testing keys()');
  console.log('--------------------------------');
  for (const s of bst.keys())
    console.log(s + ' ' + bst.get(s));
  console.log();

  // print keys in order using select
  console.log('Testing select');
  console.log('--------------------------------');
  for (let i = 0; i < bst.size; i++)
    console.log(i + ' ' + bst.select(i));
  console.log();

  // test rank, floor, ceiling
  console.log('key rank floor flor2 ceil');
  console.log('-------------------------');
  for (let i = 'A'.charCodeAt(0); i <= 'Z'.charCodeAt(0); i++) {
    const s = String.fromCodePoint(i);
    console.log('%s %d %s %s %s\n', s, bst.rank(s), bst.floor(s), bst.floor2(s), bst.ceil(s));
  }
  console.log();

  // test range search and range count
  const from = ['A', 'Z', 'X', '0', 'B', 'C'];
  const to = ['Z', 'A', 'X', 'Z', 'G', 'L'];
  console.log('range search');
  console.log('-------------------');
  for (let i = 0; i < from.length; i++) {
    console.log('%s-%s (%d) : ', from[i], to[i], bst.rangeSize(from[i], to[i]));
    for (const s of bst.rangeKeys(from[i], to[i]))
      console.log(s + ' ');
    console.log();
  }
  console.log();

  // delete the smallest keys
  for (let i = 0; i < parseInt(bst.size / 2); i++) {
    bst.deleteMin();
  }
  console.log('After deleting the smallest ' + parseInt(bst.size / 2) + ' keys');
  console.log('--------------------------------');
  for (const s of bst.keys())
    console.log(s + ' ' + bst.get(s));
  console.log();

  // delete all the remaining keys
  while (!bst.isEmpty()) {
    bst.delete(bst.select(bst.size / 2));
  }
  console.log('After deleting the remaining keys');
  console.log('--------------------------------');
  for (const s of bst.keys())
    console.log(s + ' ' + bst.get(s));
  console.log();

  console.log('After adding back the keys');
  console.log('--------------------------------');
  for (let i = 0; i < keys.length; i++) {
    bst.put(keys[i], i);
  }
  for (const s of bst.keys())
    console.log(s + ' ' + bst.get(s));
  console.log();
}

testBST();

// size = 10
// min  = A
// max  = X
//
// Testing keys()
// --------------------------------
//   A 8
// C 4
// E 12
// H 5
// L 11
// M 9
// P 10
// R 3
// S 0
// X 7
//
// Testing select
// --------------------------------
//   0 A
// 1 C
// 2 E
// 3 H
// 4 L
// 5 M
// 6 P
// 7 R
// 8 S
// 9 X
//
// key rank floor flor2 ceil
// -------------------------
//   A 0 A A A
//
// B 1 A A C
//
// C 1 C C C
//
// D 2 C C E
//
// E 2 E E E
//
// F 3 E E H
//
// G 3 E E H
//
// H 3 H H H
//
// I 4 H H L
//
// J 4 H H L
//
// K 4 H H L
//
// L 4 L L L
//
// M 5 M M M
//
// N 6 M M P
//
// O 6 M M P
//
// P 6 P P P
//
// Q 7 P P R
//
// R 7 R R R
//
// S 8 S S S
//
// T 9 S S X
//
// U 9 S S X
//
// V 9 S S X
//
// W 9 S S X
//
// X 9 X X X
//
// Y 10 X X null
//
// Z 10 X X null
//
//
// range search
// -------------------
//   A-Z (10) :
// A
// C
// E
// H
// L
// M
// P
// R
// S
// X
//
// Z-A (0) :
//
// X-X (1) :
// X
//
// 0-Z (10) :
// A
// C
// E
// H
// L
// M
// P
// R
// S
// X
//
// B-G (2) :
// C
// E
//
// C-L (4) :
// C
// E
// H
// L
//
//
// After deleting the smallest 3 keys
// --------------------------------
//   H 5
// L 11
// M 9
// P 10
// R 3
// S 0
// X 7
//
// After deleting the remaining keys
// --------------------------------
//
//   After adding back the keys
// --------------------------------
//   A 8
// C 4
// E 12
// H 5
// L 11
// M 9
// P 10
// R 3
// S 0
// X 7
