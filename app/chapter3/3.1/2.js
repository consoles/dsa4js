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
        return this._values[i];
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
