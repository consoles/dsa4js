class WordCounter {
  constructor() {
    this.wordCount = 0;
    this.lastPut = '';
    this._map = new Map();
  }

  put(key, value) {
    this.wordCount++;
    this.lastPut = key;
    this._map.set(key, value);
  }

  contains(key) {
    this.wordCount++;
    return this._map.has(key);
  }

  get(key) {
    return this._map.get(key);
  }

  keys() {
    return this._map.keys();
  }
}

module.exports = WordCounter;
