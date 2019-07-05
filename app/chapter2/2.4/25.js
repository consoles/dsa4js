function cubeSum1(n) {
  const ret = [];
  for (let a = 0; a <= n; a++) {
    for (let b = a + 1; b <= n; b++) {
      for (let c = b + 1; c <= n; c++) {
        for (let d = c + 1; d <= n; d++) {
          if (a ** 3 + b ** 3 === c ** 3 + d ** 3) {
            ret.push([a, b, c, d]);
          }
        }
      }
    }
  }
  return ret;
}

console.time('cube1');
const ret = cubeSum1(10 **4);
console.log(ret.length);
console.timeEnd('cube1');

const swap = require('../../swap');

class MinPQ {
  constructor(compareFn) {
    this.data = [-1];
    this.sz = 0;
    this.compareFn = compareFn;
  }

  less(i, j) {
    return this.compareFn(this.data[i], this.data[j]) < 0;
  }

  isEmpty() {
    return this.sz === 0;
  }

  _swim(k) {
    while (true) {
      const p = parseInt(k / 2);
      if (p < 1 || !this.less(k, p)) {
        break;
      }
      swap(this.data, p, k);
      k = p;
    }
  }

  insert(value) {
    const sz = ++this.sz;
    this.data[sz] = value;
    this._swim(sz);
  }

  _sink(k) {
    const sz = this.sz;
    while (2 * k <= sz) {
      let j = 2 * k;
      if (j + 1 <= sz && this.less(j + 1, j)) {
        j++;
      }
      if (!this.less(j, k)) {
        break;
      }
      swap(this.data, j, k);
      k = j;
    }
  }

  delMin() {
    const value = this.data[1];
    swap(this.data, 1, this.sz--);
    this._sink(1);
    return value;
  }
}

// const q = new MinPQ((a,b) => a - b);
// q.insert(2);
// q.insert(3);
// q.insert(4);
// q.insert(5);
// q.insert(1);
// q.insert(3);
// q.insert(7);
// while (!q.isEmpty()) {
//   console.log(q.delMin());
// }

class CubeSum {
  constructor(i, j) {
    this.i = i;
    this.j = j;
    this.sum = i**3 + j**3;
  }
}

const res = [];
const n = 1e4;
console.time('cube2');
const pq = new MinPQ((a,b) => a.sum - b.sum);
for (let i = 0;i <= n;i++) {
  pq.insert(new CubeSum(i,i));
}
let prev = new CubeSum(-1,-1);
let pairCount = 0;
while (!pq.isEmpty()) {
  s = pq.delMin();
  if (s.sum === prev.sum) {
    pairCount++;
    res.push([s.i,s.j,s.sum]);
  }
  if (s.j < n) {
    pq.insert(new CubeSum(s.i,s.j+1));
  }
  prev = s;
}
console.timeEnd('cube2');
console.log(res.length);
