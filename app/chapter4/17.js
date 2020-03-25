class GraphProperties {
  /**
   * 如果是非连通图将抛出异常
   * @param graph
   */
  constructor(graph) {
    const marked = new Array(graph.V).fill(false);
    this.graph = graph;
    const s = 0; // 顶点0当做起点
    marked[s] = true;
    const q = [s];
    while (q.length > 0) {
      const w = q.shift();
      for (const v of graph.adj(w)) {
        if (!marked[v]) {
          q.push(v);
          marked[v] = true;
        }
      }
    }
    for (let i = 0; i < graph.V; i++) {
      if (!marked[i]) {
        throw new Error('非连通图');
      }
    }
  }

  /**
   * 图的周长：图中最短环的长度，如果是无环图，则它的周长为无穷大
   */
  girth() {

  }
}
