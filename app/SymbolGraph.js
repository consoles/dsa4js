const Graph = require('./Graph');

/**
 * 符号图:用例使用字符串代替数字索引来表示图中的顶点
 */
class SymbolGraph {
  constructor(lines, delimiter) {
    this.st = new Map(); // 符号名 -> 索引
    this.keys = []; // 索引 -> 符号名（即：st的反向索引）
    this.graph = null; // 图

    const edgesStr = [];

    // 第一遍：构造索引
    for (const line of lines) {
      const items = line.split(delimiter);
      edgesStr.push(items);
      for (const item of items) {
        if (!this.st.has(item)) {
          this.st.set(item, this.st.size); // 为每个不同的字符串关联一个索引
        }
      }
    }
    // 构建反向索引
    for (const [name, index] of this.st) {
      this.keys[index] = name;
    }

    // 第二遍：构造图
    const edges = edgesStr.map(x => x.map(t => this.st.get(t)));
    this.graph = new Graph(this.st.size, edges);
  }

  contains(s) {
    return this.st.has(s);
  }

  index(s) {
    return this.st.get(s);
  }

  name(v) {
    return this.keys[v];
  }

  G() {
    return this.graph;
  }
}
