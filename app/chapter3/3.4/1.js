function hash(char) {
  return (char.toUpperCase().charCodeAt(0) - 65) * 11 % 5;
}

for (const char of 'EASYQUESTION'.split('')) {
  console.log(char, hash(char));
}

// E 4
// A 0
// S 3
// Y 4
// Q 1
// U 0
// E 4
// S 3
// T 4
// I 3
// O 4
// N 3
