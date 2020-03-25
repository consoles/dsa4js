function getAll(arr) {
  const ret = [];

  function dfs(index, items) {
    if (index === items.length) {
      return ret.push(items.slice());
    }
    for (let i = index; i < items.length; i++) {
      [items[i], items[index]] = [items[index], items[i]];
      dfs(index + 1, items);
      [items[i], items[index]] = [items[index], items[i]];
    }
  }

  dfs(0, arr);
  return ret;
}

const arr = [
  [0, 1],
  [1, 2],
  [2, 3],
  [3, 0]
];

const seq = getAll(arr);

const Graph = require('../Graph');

const set = new Set();

for (const edges of seq) {
  const g = new Graph(4, edges);
  set.add(JSON.stringify(g._adj));
}

const matrix = [
  [1, 2],
  [0, 2],
  [1, 3],
  [0, 2]
];

function* getAllMatrix(matrix) {
  function* getAll(index) {
    if (index === matrix.length) {
      return;
    }
    yield matrix;
    [[matrix[index][0]], [matrix[index][1]]] = [[matrix[index][1]], [matrix[index][0]]];
    yield matrix;
    [[matrix[index][0]], [matrix[index][1]]] = [[matrix[index][1]], [matrix[index][0]]];
    yield* getAll(index + 1);
  }

  yield* getAll(0);
}

const s2 = new Set();

for (const s of getAllMatrix(matrix)) {
  s2.add(JSON.stringify(s));
}

debugger

for (const s of s2) {
  if (!set.has(s)) {
    console.log(s);
  }
}

// [[1,2],[0,2],[1,3],[0,2]]
// [[2,1],[0,2],[1,3],[0,2]]
// [[1,2],[2,0],[1,3],[0,2]]
// [[1,2],[0,2],[3,1],[0,2]]
// [[1,2],[0,2],[1,3],[2,0]]
