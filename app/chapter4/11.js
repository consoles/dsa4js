const Graph = require('../Graph');

function bfs(graph, s) {
  const edgeTo = new Array(graph.V);
  const marked = new Array(graph.V).fill(false);
  const q = [s];
  marked[s] = true;
  while (q.length > 0) {
    const v = q.shift();
    for (const w of graph.adj(v)) {
      if (!marked[w]) {
        marked[w] = true;
        q.push(w);
        edgeTo[w] = v;
      }
    }
  }
  return  edgeTo;
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
const edgeTo  = bfs(g,0);
console.log(edgeTo);

// [ <1 empty item>, 0, 0, 5, 6, 0, 0, <6 empty items> ]
