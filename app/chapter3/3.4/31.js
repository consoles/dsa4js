const LinearProbingHashST = require('../LinearProbingHashST');

class SymbolTable {
  constructor() {
    const st1 = new LinearProbingHashST();
    const st2 = new LinearProbingHashST();
    this.st1 = st1;
    this.st2 = st2;
  }

  put(key, value) {
    const exist1 = this.st1.contains(key);
    if (exist1) {
      const oldValue1 = this.st1.get(key);
      const exist2 = this.st2.contains(key);
      if (exist2) {
        const oldValue2 = this.st2.get(key);
        this.st1.put(key, oldValue2);
        // st2位置腾出来
        this.st2.delete(key);
      } else {
        this.st2.put(key, oldValue1);
      }
    } else {
      this.st1.put(key, value);
    }
  }
}
