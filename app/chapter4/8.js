// 使用union found算法实现搜索api

const Graph = require('../Graph');

class Search {
  /**
   * 找到和s相连的所有顶点
   * @param graph
   * @param s
   */
  constructor(graph, s) {
    this.graph = graph;
    this.s = s;
    this.c = 0;
  }

  _dfs(v, mark) {
    mark[v] = true;
    for (const w of this.graph.adj(v)) {
      if (w === this.s) {
        return true;
      }
      if (!mark[w]) {
        const flag = this._dfs(w, mark);
        if (flag) {
          return flag;
        }
      }
    }
    return false;
  }

  /**
   * v和s是连通的么
   * @param v
   */
  marked(v) {
    const mark = new Array(this.graph.V).fill(false);
    return v === this.s || this._dfs(v, mark);
  }

  markedBFS(v) {
    const q = [];
    q.push(this.s);
    const mark = new Array(this.graph.V).fill(false);
    while (q.length > 0) {
      const vv = q.shift();
      if (vv === v) {
        return true;
      }
      for (const w of this.graph.adj(vv)) {
        if (!mark[w]) {
          q.push(w);
          mark[vv] = true;
        }
      }
    }
    return false;
  }

  _count(v, mark) {
    mark[v] = true;
    for (const w of this.graph.adj(v)) {
      if (!mark[w]) {
        this.c++;
        this._count(w, mark);
      }
    }
  }

  /**
   * 与s连通的顶点总数
   */
  count() {
    const mark = new Array(this.graph.V).fill(false);
    this.c = 0;
    this._count(this.s, mark);
    return this.c;
  }

  countBFS() {
    const mark = new Array(this.graph.V).fill(false);
    const q = [];
    q.push(this.s);
    mark[this.s] = true;
    let c = 0;
    while (q.length > 0) {
      const vv = q.shift();
      for (const w of this.graph.adj(vv)) {
        if (!mark[w]) {
          c++;
          q.push(w);
          mark[w] = true;
        }
      }
    }
    return c;
  }
}

class UnionFoundSearchQuickFind {
  constructor(v, edges, s) {
    this.v = v;
    this.s = s;
    const id = [];
    for (let i = 0; i < this.v; i++) {
      id[i] = i;
    }
    this.id = id;
    for (const [startV, endV] of edges) {
      this.union(startV, endV);
    }
  }

  union(p, q) {
    const pID = this.find(p);
    const qID = this.find(q);
    if (pID === qID) return;
    for (let i = 0; i < this.id.length; i++) {
      if (this.id[i] === q) {
        this.id[i] = pID;
      }
    }
  }

  find(p) {
    return this.id[p];
  }

  marked(v) {
    return this.find(v) === this.find(this.s);
  }

  count() {
    return this.id.filter(x => x === this.s).length - 1;
  }
}

class UnionFindSearchQuickUnion extends UnionFoundSearchQuickFind {
  union(p, q) {
    const pRoot = this.find(p);
    const qRoot = this.find(q);
    if (pRoot === qRoot) return;
    this.id[qRoot] = this.id[pRoot];
  }

  find(p) {
    while (p !== this.id[p]) {
      p = this.id[p];
    }
    return p;
  }

  count() {
    // 遍历 id数组，找出以该节点为根的节点（直接根节点和间接根节点）
    const root = this.find(this.s);
    return this.id.filter(x => this.find(x) === root).length - 1;
  }
}

class UnionFindSearchWeightQuickUnion extends UnionFindSearchQuickUnion {
  constructor(v, edges, s) {
    super(v, edges, s);
    this.sz = new Array(v).fill(1);
  }

  union(p, q) {
    const pRoot = this.find(p);
    const qRoot = this.find(q);
    // 将小树合并到大树
    if (this.sz[pRoot] < this.sz[qRoot]) {
      this.id[pRoot] = this.id[qRoot];
      this.sz[qRoot] += this.sz[pRoot];
    } else {
      this.id[qRoot] = this.id[pRoot];
      this.sz[pRoot] += this.sz[qRoot];
    }
  }
}

class PathCompressQuickUnion extends UnionFindSearchWeightQuickUnion {
  find(p) {
    const root  = super.find(p);
    while (p !== root) {
      const parent = this.id[p];
      this.id[p] = root;
      p = parent;
    }
    return root;
  }
}

const V = 13;
const edges = [
  [0, 5],
  [4, 3],
  [0, 1],
  [9, 12],
  [6, 4],
  [5, 4],
  [0, 2],
  [11, 12],
  [9, 10],
  [0, 6],
  [7, 8],
  [9, 11],
  [5, 3]
];

const g = new Graph(V, edges);
let search = new Search(g, 0);
console.log('dfs');
for (let v = 0; v < g.V; v++) {
  if (search.marked(v)) {
    process.stdout.write(v + '\t');
  }
}
console.log();
console.log(search.count(), g.V);

console.log('=============');

console.log('bfs');
for (let v = 0; v < g.V; v++) {
  if (search.markedBFS(v)) {
    process.stdout.write(v + '\t');
  }
}
console.log();
console.log(search.countBFS(), g.V);

console.log('--------------');

search = new UnionFoundSearchQuickFind(V, edges, 0);
console.log('quick find');
for (let v = 0; v < g.V; v++) {
  if (search.marked(v)) {
    process.stdout.write(v + '\t');
  }
}
console.log();
console.log(search.count(), V);

search = new UnionFindSearchQuickUnion(V, edges, 0);
console.log('quick union');
for (let v = 0; v < g.V; v++) {
  if (search.marked(v)) {
    process.stdout.write(v + '\t');
  }
}
console.log();
console.log(search.count(), V);

search = new UnionFindSearchQuickUnion(V, edges, 0);
console.log('加权quick union');
for (let v = 0; v < g.V; v++) {
  if (search.marked(v)) {
    process.stdout.write(v + '\t');
  }
}
console.log();
console.log(search.count(), V);

search = new UnionFindSearchQuickUnion(V, edges, 0);
console.log('基于路径压缩的 quick union');
for (let v = 0; v < g.V; v++) {
  if (search.marked(v)) {
    process.stdout.write(v + '\t');
  }
}
console.log();
console.log(search.count(), V);
