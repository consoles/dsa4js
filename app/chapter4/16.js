/**
 * 顶点v的离心率：它和离它最远的顶点的最短距离
 */
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
   * v的离心率
   */
  eccentricity(v) {
    // 以顶点v为起点,进行广度优先搜索（广度优先可以保证距离最短）
    const marked = [];
    const edgeTo = [];
    marked[v] = true;
    const q = [v];
    while (q.length > 0) {
      const w = q.shift();
      for (const v of this.graph.adj(w)) {
        if (!marked[v]) {
          q.push(v);
          edgeTo[v] = w;
          marked[v] = true;
        }
      }
    }

    const dis = [];
    for (let i = 0; i < this.graph.V; i++) {
      let d = 0;
      for (let x = i; x !== v; x = edgeTo[x]) {
        d++;
      }
      dis[i] = d;
    }
    return Math.max.apply(null, dis);
  }

  /**
   * g的直径：所有顶点的最大离心率
   */
  diameter() {
    const eccs  = [];
    for (let i = 0;i < this.graph.V;i++) {
      eccs.push(this.eccentricity(i));
    }
    return Math.max.apply(null,eccs);
  }

  /**
   * g的半径：所有顶点的最小离心率
   */
  radius() {
    const eccs  = [];
    for (let i = 0;i < this.graph.V;i++) {
      eccs.push(this.eccentricity(i));
    }
    return Math.min.apply(null,eccs);
  }

  /**
   * g的某个中点：离心率和半径相等的顶点
   */
  center() {
    const points  = [];
    const r = this.radius();
    for (let i = 0;i < this.graph.V;i++) {
      if (this.eccentricity(i) === r) {
        points.push(i);
      }
    }
    return points;
  }
}

const Graph = require('../Graph');

const V = 7;

const edges = [
  [0, 5],
  [4, 3],
  [0, 1],
  [6, 4],
  [5, 4],
  [0, 2],
  [5, 3]
];
const g = new Graph(V, edges);

const p = new GraphProperties(g);
console.log(p.eccentricity(0));
console.log(p.diameter());
console.log(p.radius());
console.log(p.center());
