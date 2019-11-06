const _ = require('lodash');

class RandomAccessMap {

  constructor() {
    this.map = new Map();
    this.keys = new Set();
  }

  put(key, value) {
    if (!this.map.has(key)) {
      this.keys.add(key);
    }
    this.map.set(key, value);
  }

  get(key) {
    return this.map.get(key);
  }

  /**
   * 删除并返回一个随机的键
   */
  delete() {
    const key = _.sample([...this.keys]);
    this.keys.delete(key);
    const ret = this.map.get(key);
    this.map.delete(key);
    return ret;
  }
}

const r = new RandomAccessMap();
r.put(1,11);
r.put(2,22);
r.put(3,33);
r.put(4,44);

console.log(r.get(1));
console.log(r.delete(2));
console.log(r.get(3));
