/**
 * 深度优先搜索找出图中的所有连通分量
 */
class CC {
  constructor(graph) {
    this.marked = new Array(graph.V).fill(false);
    this._id = new Array(graph.V);
    this.count = 0;
    for (let s = 0; s < graph.V; s++) {
      if (!this.marked[s]) {
        this.dfs(graph, s);
        this.count++;
      }
    }
  }

  dfs(graph, v) {
    this.marked[v] = true;
    this._id[v] = this.count;
    for (const w of graph.adj(v)) {
      if (!this.marked) {
        this.dfs(graph, w);
      }
    }
  }

  connected(v, w) {
    return this._id[v] = this._id[w];
  }

  id(v) {
    return this._id[v];
  }
}

module.exports = CC;
