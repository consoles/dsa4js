/**
 * 使用广度优先搜索寻找图中的路径
 */
class BFSPaths {
  constructor(graph, s) {
    this.marked = new Array(graph.V).fill(false);
    this.edgeTo = [];
    this.s = s;
    this.bfs(graph, s);
  }

  bfs(graph, v) {
    const queue = [];
    this.marked[x] = true;
    queue.push(v);
    while (queue.length > 0) {
      const v = queue.shift();
      for (const w of graph.adj(v)) {
        if (!this.marked[w]) {
          queue.push(w);
          this.edgeTo[w] = v;
          this.marked[w] = true;
        }
      }
    }
  }

  hashPathTo(v) {
    return this.marked[v];
  }

  pathTo(v) {
    const stack = [];
    for (let x = v; x !== this.s; x = this.edgeTo[x]) {
      stack.push(x);
    }
    stack.push(this.s);
    return stack;
  }
}

module.exports = BFSPaths;
