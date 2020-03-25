/**
 * 使用DFS检测图中是否有环路
 */
class Cycle {
  constructor(graph) {
    this.marked = new Array(graph.V).fill(false);
    this.hasCycle = false;
    for (let s = 0; s < graph.V; s++) {
      if (!this.marked[s]) {
        console.log('s = ', s);
        this.dfs(graph, s, s);
      }
    }
  }

  /**
   * @param graph
   * @param v 当前要访问的节点
   * @param u 当前要访问节点的父节点（也就是上次访问的节点）
   *
   * 核心思想：在dfs的过程中哦哦那个发现某个顶点有一条边指向已经访问过的顶点，且这个顶点不是当前顶点的父节点，则说明图中包含环路
   */
  dfs(graph, v, u) {
    this.marked[v] = true;
    for (const w of graph.adj(v)) {
      console.log('v =', v, 'w =', w, 'u =', u, `marked[${w}] = ${this.marked[w]}`);
      if (!this.marked[w]) {
        this.dfs(graph, w, v);
      } else if (w !== u) {
        this.hasCycle = true;
      }
    }
  }
}

// test
const Graph = require('../app/Graph');

const g = new Graph(3, [[0, 1], [0, 2], [1, 2]]);
g.print();
const c = new Cycle(g);
console.log(c.hasCycle);
