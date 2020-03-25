// 不允许存在自环和平行边的图
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
    if (this._adj[startV].includes(endV)) {
      throw new Error(`存在平行边 ${startV} -> ${endV}`);
    }
    if (this._adj[endV].includes(startV)) {
      throw new Error(`存在平行边 ${endV} -> ${startV}`);
    }
    if (startV === endV) {
      throw new Error(`存在自环 ${startV} -> ${endV}`);
    }

    this._adj[startV].unshift(endV);
    this._adj[endV].unshift(startV);
    this.E++;
  }

  hasEdge(v, w) {
    return this.adj(v).includes(w);
  }
}
