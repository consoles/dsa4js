const Graph = require('../Graph');

function dfs(graph, s, edgeTo = [], marked = new Array(graph.V).fill(false)) {
  marked[s] = true;
  for (const w of graph.adj(s)) {
    if (!marked[w]) {
      edgeTo[w] = s;
      dfs(graph, w, edgeTo, marked);
    }
  }
}

const V = 13;
const edgeTo = new Array(V);
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
dfs(g, 0, edgeTo);
console.log(edgeTo);

// [ <1 empty item>, 0, 0, 5, 6, 4, 0, <6 empty items> ]
