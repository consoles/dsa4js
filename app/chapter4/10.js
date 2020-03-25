// 证明在任意一幅联通图中都存在一个顶点，删除它（以及它所连接的所有边）不会影响到图的连通性，编写一个DFS的方法找出这样一个顶点，提示：留意那些相邻顶点全部都被标记过的顶点

const Graph = require('../Graph');

const V = 13;
const edges = [
  [0, 5],
  [4, 3],
  [0, 1],
  [6, 4],
  [5, 4],
  [0, 2],
  [0, 6],
  [5, 3]
];

const g = new Graph(V, edges);

function dfs(graph, v, marked = new Array(graph.V).fill(false)) {
  marked[v] = true;
  const total = graph.adj(v).length;
  let count = 0;
  for (const w of graph.adj(v)) {
    if (!marked[w]) {
      dfs(graph, w, marked);
    } else {
      count++;
    }
  }
  if (count === total) {
    console.log(`去掉顶点${v}不影响图的连通性`);
  }
}

dfs(g, 0);
