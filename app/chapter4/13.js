const Graph = require('../Graph');

class BFSPaths {
  constructor(graph, s) {
    this.dist = new Array(graph.V).fill(-1);
    this.bfs(graph, s);
  }

  bfs(graph, s) {
    const q = [s];
    const marked = new Array(graph.V).fill(false);
    marked[s] = true;
    const edgeTo = [];
    while (q.length > 0) {
      // 广度优先搜索一定要用队列
      const v = q.shift();
      for (const w of graph.adj(v)) {
        if (!marked[w]) {
          marked[w] = true;
          q.push(w);
          edgeTo[w] = v;
        }
      }
    }
    this.dist[s] = 0;
    for (let i = 0; i < edgeTo.length; i++) {
      let e = edgeTo[i];
      if (typeof e !== 'number') continue;
      let dis = 0;
      do {
        dis++;
        e = edgeTo[e];
      } while (e === s);
      this.dist[i] = dis;
    }
  }

  // O(1)时间求得起点到给定顶点的最短距离
  distTo(v) {
    return this.dist[v];
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
const paths = new BFSPaths(g, 0);
for (let i  = 0;i < V;i++) {
  console.log(paths.distTo(i));
}
