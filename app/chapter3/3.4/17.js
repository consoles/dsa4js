const LinearProbingHashST = require('../LinearProbingHashST');

class Char {
  constructor(key, index) {
    this.key = key;
    this.index = index;
  }

  hashCode() {
    return this.key.charCodeAt(0);
  }

  equals(other) {
    return this.key === other.key;
  }
}

const items = 'ABCDE'.split('').map((x, i) => new Char(x, i));

const st = new LinearProbingHashST(3);

for (let i = 0; i < items.length; i++) {
  st.put(items[i], i);
}

debugger;

// st.delete(items[2]);

// 0 1 2 3 4 5 6 7 8 9 10 11 12
//           A B C D E
//  删除后(7位置为null)
//           A B   D E

// 被删除位置后的元素会被先清空，后重新插入（不能直接删除的原因是可能后面的元素和倍删除元素hash冲突，再次put的话如果hash冲突的话会被放到正确的位置）

for (const key of st.keys()) {
  console.log(key);
}

debugger;
