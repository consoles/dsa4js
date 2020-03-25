class DFS {
  constructor(graph, s) {
    this.marked = new Array(graph.V).fill(false);
    this.count = 0;
    this.dfs(graph, s);
  }

  dfs(graph, v) {
    this.marked[v] = true;
    for (const w of graph.arj[v]) {
      if (!this.marked[v]) {
        this.dfs(graph, w);
      }
    }
  }
}

module.exports = DFS;
