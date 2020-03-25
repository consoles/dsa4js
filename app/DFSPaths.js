/**
 * 使用深度优先搜索查找图中的路径
 */
class DFSPaths {
  constructor(graph, s) {
    this.marked = new Array(graph.V).fill(false);
    this.edgeTo = new Array(graph.V); // 从起点到一个顶点的已知路径上的最后一个顶点
    this.s = s; // 起点
    this.dfs(graph, s);
  }

  dfs(graph, v) {
    this.marked[v] = true;
    for (const w of graph.adj(v)) {
      if (!this.marked[w]) {
        this.edgeTo[w] = v;
        this.dfs(graph, w);
      }
    }
  }

  hasPathTo(v) {
    return this.marked[v];
  }

  pathTo(v) {
    if (!this.hasPathTo(v)) return null;
    const stack = [];
    for (let x = v; x !== this.s; x = this.edgeTo[x]) {
      stack.push(x);
    }
    stack.push(this.s);
    return stack;
  }
}

module.exports = DFSPaths;
