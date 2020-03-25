const Graph = require('../Graph');

Graph.prototype.dump = function () {
  const visited = {};
  console.log(`${this.V} 个顶点，${this.E} 条边`);
  for (let i = 0; i < this._adj.length; i++) {
    visited[i] = visited[i] || {};
    const items = [];
    for (let j = 0; j < this._adj[i].length; j++) {
      const v = this._adj[i][j];
      let item = v;
      visited[v] = visited[v] || {};
      if (visited[i][v] || visited[v][i]) {
        item = `<${v}>`;
      } else {
        visited[i][v] = true;
        visited[v][i] = true;
      }
      items.push(item);
    }
    console.log(`${i}: ${items.join(' ')}`);
  }
};

const data = [
  13,
  13,
  [0, 1, 2, 5, 6],
  [3, 4, 5],
  [4, 5, 6],
  [7, 8],
  [9, 10, 11, 12],
  [11, 12]
];

const V = data[0];
const E = data[1];

const edges = [];

for (let i = 2; i < data.length; i++) {
  const startV = data[i][0];
  for (let j = 1; j < data[i].length; j++) {
    const endV = data[i][j];
    edges.push([startV, endV]);
  }
}

const g = new Graph(V, edges);
g.dump();
