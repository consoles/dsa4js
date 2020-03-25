/**
 * 双色问题：使用DFS检测图是否为二分图
 */
class TwoColor {
  constructor(graph) {
    this.marked = new Array(graph.V()).fill(false);
    this.color = new Array(graph.V()).fill(false);
    this.isTwoColorable = true;
    for (let s = 0; s < graph.V(); s++) {
      if (!this.marked[s]) {
        this.dfs(graph, s);
      }
    }
  }

  dfs(graph, v) {
    this.marked[v] = true;
    for (const w of graph.adj(v)) {
      if (!this.marked[w]) {
        this.color[w] = !this.color[w];
        this.dfs(graph, w);
      } else if (this.color[w] === this.color[v]) {
        this.isTwoColorable = false;
        break;
      }
    }
  }
}
