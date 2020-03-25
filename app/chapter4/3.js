// 由于js中构造函数不能重载，因此使用一个新的方法，名称为clone

class Graph {
  constructor(v, edges) {
    this.V = v;
    this.E = 0;
    this._adj = [];
    for (let i = 0; i < v; i++) {
      this._adj[i] = [];
    }
    for (const [startV, endV] of edges) {
      this.addEdge(startV, endV);
    }
  }

  adj(v) {
    return this._adj[v];
  }

  static _cloneDFS(g, marked, s, graph) {
    marked[s] = true;
    for (const w of g.adj(s)) {
      if (!marked[w]) {
        graph.addEdge(s, w);
        Graph._cloneDFS(g, marked, w, graph);
      }
    }
  }

  static cloneDFS(g) {
    const marked = new Array(g.V);
    const s = g.adj(0)[0];
    const graph = new Graph(g.V,[]);
    Graph._cloneDFS(g, marked, s, graph);
    return graph;
  }

  static cloneBFS(g) {
    const marked = new Array(g.V);
    const graph = new Graph(g.V, []);
    // BFS
    const s = g.adj(0)[0];
    const q = [];
    q.push(s);
    marked[s] = true;
    while (q.length > 0) {
      const startV = q.shift();
      for (const w of g.adj(startV)) {
        if (!marked[w]) {
          marked[w] = true;
          graph.addEdge(startV, w);
          q.push(w);
        }
      }
    }
    return graph;
  }

  addEdge(startV, endV) {
    if (!this._adj[startV].includes(endV)) {
      this._adj[startV].unshift(endV);
    }
    if (!this._adj[endV].includes(startV)) {
      this._adj[endV].unshift(startV);
    }
    this.E++;
  }

  /**
   * 打印出图的邻接表
   */
  print() {
    for (let i = 0; i < this._adj.length; i++) {
      console.log(i, this._adj[i].join(' -> '));
    }
  }
}

const edges = [
  [0, 1], [0, 2], [0, 5],
  [2, 3], [2, 4],
];

const v = 6;

const g1 = new Graph(v, edges);
g1.print();
console.log('BFS克隆图');
const g2 = Graph.cloneBFS(g1);
g2.print();
console.log('DFS克隆图');
const g3 = Graph.cloneDFS(g1);
g3.print();
