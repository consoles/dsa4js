/**
 * 无向图
 */
class Graph {
  /**
   * @param v 顶点数
   * @param edges 边[[0,1],[1,2],[2,3]]
   */
  constructor(v, edges) {
    this.V = v; // 顶点数
    this.E = 0; // 边数
    const adj = []; // 邻接表
    for (let i = 0; i < v; i++) {
      adj[i] = [];
    }
    this._adj = adj;

    // 构造边
    for (const [startV, endV] of edges) {
      this.addEdge(startV, endV);
    }
  }

  /**
   * 添加一条边
   * @param startV 起点
   * @param endV 终点
   */
  addEdge(startV, endV) {
    if (!this._adj[startV].includes(endV)) {
      this._adj[startV].unshift(endV);
    }
    if (!this._adj[endV].includes(startV)) {
      this._adj[endV].unshift(startV);
    }
    this.E++;
  }

  adj(v) {
    return this._adj[v];
  }

  print(){
    for (let i = 0;i < this._adj.length;i++) {
      console.log(i,this.adj(i).join(' -> '));
    }
  }

}

module.exports = Graph;
