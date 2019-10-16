const _ = require('lodash');

class LinearProbingHashST38 {
  constructor(M = 16) {
    this.N = 0; // 键值对数目
    this.M = M; // 线性探测表的大小（需要始终保证 N <= 1/2M）
    // 保存键值对的平行数组
    this._keys = new Array(M);
    this._values = new Array(M);
    this.testCount = 0;
  }

  _resize(cap) {
    const t = new LinearProbingHashST38(cap);
    for (let i = 0; i < this._keys.length; i++) {
      if (this._keys[i]) {
        t.put(this._keys[i], this._values[i]);
      }
    }
    this._keys = t._keys;
    this._values = t._values;
    this.M = cap;
  }

  _hash(key) {
    return (key.hashCode() & 0x7fffffff) % this.M;
  }

  put(key, value) {
    if (this.M <= 2 * this.N) {
      this._resize(2 * this.M);
    }
    let i = this._hash(key);
    // 探测
    for (; this._keys[i]; i = (i + 1) % this.M) {
      this.testCount++;
      // 覆盖
      if (key.equals( this._keys[i]) ){
        this._values[i] = value;
        return;
      }
    }
    // 找到空位，直接赋值
    this._keys[i] = key;
    this._values[i] = value;
    this.N++;
  }
}

function buildArr() {
  const arr = [];
  for (let i = 0; i < 1e5; i++) {
    arr.push(i);
  }
  return _.shuffle(arr);
}

class Int {
  constructor(value) {
    this.value = value;
  }

  hashCode() {
    return this.value & 0x7fffffff;
  }
  equals(other) {
    return this.value === other.value;
  }
}

const arr = buildArr();

const st = new LinearProbingHashST38();
for (let i = 0; i < arr.length; i++) {
  st.put(new Int(arr[i]), i);
  if (i % 1000 === 0) {
    console.log(i, st.testCount);
  }
}

// 0 0
// 1000 771
// 2000 1524
// 3000 2029
// 4000 3096
// 5000 3622
// 6000 4235
// 7000 5041
// 8000 6073
// 9000 6497
// 10000 6871
// 11000 7269
// 12000 7739
// 13000 8277
// 14000 8901
// 15000 9655
// 16000 10601
// 17000 11072
// 18000 11337
// 19000 11608
// 20000 11900
// 21000 12217
// 22000 12601
// 23000 13032
// 24000 13473
// 25000 14032
// 26000 14523
// 27000 15151
// 28000 15839
// 29000 16647
// 30000 17414
// 31000 18422
// 32000 19391
// 33000 20386
// 34000 20386
// 35000 20386
// 36000 20386
// 37000 20386
// 38000 20386
// 39000 20386
// 40000 20386
// 41000 20386
// 42000 20386
// 43000 20386
// 44000 20386
// 45000 20386
// 46000 20386
// 47000 20386
// 48000 20386
// 49000 20386
// 50000 20386
// 51000 20386
// 52000 20386
// 53000 20386
// 54000 20386
// 55000 20386
// 56000 20386
// 57000 20386
// 58000 20386
// 59000 20386
// 60000 20386
// 61000 20386
// 62000 20386
// 63000 20386
// 64000 20386
// 65000 20386
// 66000 20386
// 67000 20386
// 68000 20386
// 69000 20386
// 70000 20386
// 71000 20386
// 72000 20386
// 73000 20386
// 74000 20386
// 75000 20386
// 76000 20386
// 77000 20386
// 78000 20386
// 79000 20386
// 80000 20386
// 81000 20386
// 82000 20386
// 83000 20386
// 84000 20386
// 85000 20386
// 86000 20386
// 87000 20386
// 88000 20386
// 89000 20386
// 90000 20386
// 91000 20386
// 92000 20386
// 93000 20386
// 94000 20386
// 95000 20386
// 96000 20386
// 97000 20386
// 98000 20386
// 99000 20386
