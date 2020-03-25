class Graph {
  constructor(V, edges = []) {
    this.V = V;
    this.E = 0;
    this._adj = [];
    for (let i = 0; i < V; i++) {
      this._adj[i] = [];
    }
    for (const [startV, endV] of edges) {
      this.addEdge(startV, endV);
    }
  }

  adj(v) {
    return this._adj[v];
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

  hasEdge(v, w) {
    return this.adj(v).includes(w);
  }
}

const edges = [
  [0, 1], [0, 2], [0, 5],
  [2, 3], [2, 4],
];

const v = 6;

const g = new Graph(v, edges);
console.log(g.hasEdge(3, 2));
console.log(g.hasEdge(3, 4));
