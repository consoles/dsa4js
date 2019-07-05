const swap = require('../../swap');

class MinPQ {
  constructor() {
    this.sz = 0;
    this.data = [-1];
  }

  _sink(k) {
    const sz = this.sz;
    while (2 * k <= sz) {
      let j = 2 * k;
      if (j < sz && this.data[j + 1] < this.data[j]) {
        j++;
      }
      if (this.data[j] >= this.data[k]) {
        break;
      }
      swap(this.data, k, j);
      k = j;
    }
  }

  _swim(k) {
    if (k === 1) return;
    const paths = []; // 当前节点、父亲节点、爷爷节点、祖父节点，值依次减少
    let tmp = k;
    while (tmp >= 1) {
      paths.push(tmp);
      tmp = parseInt(tmp / 2);
    }
    let lo = 1; // 插入节点的父节点
    let hi = paths.length - 1; // 根节点
    while (lo <= hi) {
      const mid = lo + parseInt((hi - lo) / 2);
      if (this.data[k] > this.data[paths[mid]]) {
        hi = mid - 1;
      } else {
        lo = mid + 1;
      }
    }
    for (let i = 2; i < lo; i++) {
      swap(this.data, paths[i - 1], paths[i]); // 1,0,2,1
    }
  }

  insert(value) {
    const sz = ++this.sz;
    this.data[sz] = value;
    this._swim(sz);
  }

  delMin() {
    const value = this.data[1];
    const sz = this.sz--;
    swap(this.data, 1, sz);
    this.data[sz] = null;
    this._sink(1);
    return value;
  }

  isEmpty() {
    return this.sz === 0;
  }
}

const q = new MinPQ();
q.insert(1);
q.insert(3);
q.insert(2);
q.insert(4);
q.insert(5);
q.insert(7);

while (!q.isEmpty()) {
  console.log(q.delMin());
}

// 从根结点到某一个叶子结点的路径是有序的，满足二分查找的条件。
// 从叶子结点到根结点的路径可以通过不断地令 k = k / 2 得到（从下往上只有一条路径）。
// 但从根结点到叶子结点的路径却不能简单地通过 k = k * 2 得到（从上往下会有两条分支）。
// 因此只通过堆本身是无法满足二分查找对于随机访问的要求的。
// 为了达到 ~loglogN 次比较,先通过一个数组来保存路径，再对这个数组进行二分查找，从而获得合适的祖先结点。路径的长度是 ~logN（完全二叉树的性质），于是二分查找的比较次数即为 ~loglogN
// 删除操作原本就是 ~2logN 的，不需要修改。
// 注意这样的方法仅仅只是减少了比较次数，为了保持堆的有序，即使找到了结点的合适位置也不能直接插入，仍然需要将路径上的结点依次下移，空出位置后再插入结点，复杂度仍然是 ~logN。由于增加了保存路径等操作（建立了大量的小数组），实际算法的运行时间是增加的。
