// 自组织查找。自组织查找是一种能够将数组元素重新排序使得被访问频率较高的元素更容易被找到的查找算法。以下的实现在每次查找命中的时候，将被查找到的键值移动到数组的开头，将所有中间的键值对向右移动一格。这个启发式的过程被称为前移编码

class ArrayST {
  constructor() {
    this.sz = 0;
    this._keys = [];
    this._values = [];
  }

  put(key, value) {
    for (let i = 0; i < this.sz; i++) {
      if (this._keys[i] === key) {
        this._values[i] = value;
        return;
      }
    }
    const sz = this.sz++;
    this._keys[sz] = key;
    this._values[sz] = value;
  }

  get(key) {
    for (let i = 0; i < this.sz; i++) {
      if (this._keys[i] === key) {
        const value = this._values[i];
        for (let j = i;j > 0;j--) {
          this._keys[j] = this._keys[j-1];
          this._values[j] = this._values[j-1];
        }
        this._keys[0] = key;
        this._values[0] = value;
        return value;
      }
    }
    return null;
  }

  delete(key) {
    let index = -1;
    const lastIndex = this.sz - 1;
    for (let i = 0; i <= lastIndex; i++) {
      if (this._keys[i] === key) {
        index = i;
        break;
      }
    }
    if (index !== -1) {
      this._keys[index] = this._keys[lastIndex];
      this._values[index] = this._values[lastIndex];
      this._keys[lastIndex] = null;
      this._values[lastIndex] = null;
      this.sz--;
    }
  }

  get size() {
    return this.sz;
  }
}

const keys = 'SEARCHEXAMPLE'.split('');
const st = new ArrayST();
for (let i = 0; i < keys.length; i++) {
  st.put(keys[i], i);
}
st.get('A');
debugger;
