# 图

## 图的几种表示方法

假设图G具有V个顶点E条边。

### 邻接矩阵

可以使用一个V*V的布尔矩阵。当顶点i和顶点j之间有连接的边的时候元素值为true，否则为false。

![图的邻接矩阵表示1.png](http://git-hexo-blog.oss-cn-beijing.aliyuncs.com/%E5%9B%BE%E7%9A%84%E9%82%BB%E6%8E%A5%E7%9F%A9%E9%98%B5%E8%A1%A8%E7%A4%BA1.png)

举例如下：

![图的邻接矩阵实例.png](http://git-hexo-blog.oss-cn-beijing.aliyuncs.com/%E5%9B%BE%E7%9A%84%E9%82%BB%E6%8E%A5%E7%9F%A9%E9%98%B5%E5%AE%9E%E4%BE%8B.png)

邻接矩阵的表示占用的空间比较大，因为实际应用中上百万个节点的稀疏图是很常见的。邻接矩阵可以存储有向图，不能存储平行边。

### 邻接表

将每个顶点的所有相邻节点都保存在该顶点对应的元素所指向的一组链表中。

![无向图的表示.jpg](http://git-hexo-blog.oss-cn-beijing.aliyuncs.com/%E6%97%A0%E5%90%91%E5%9B%BE%E7%9A%84%E8%A1%A8%E7%A4%BA.jpg)

![有向图.jpg](http://git-hexo-blog.oss-cn-beijing.aliyuncs.com/%E6%9C%89%E5%90%91%E5%9B%BE.jpg)

这是**稀疏图的标准表示形式**。要添加一条v到w的边需要将w添加到v的邻接表中，对于无向图我们还需要将w添加到v的邻接表中，因此对于无向图，邻接表表示的**每条边会出现2次**。这种数据结构实现的图具有以下特点：

- 使用的空间和V+E成正比
- 添加一条边所需要的时间为常数
- 遍历顶点v的所有相邻节点和v的度数成正比（处理每个相邻节点的时间为常数）

##无向图的实现

```js
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
  
}
```

## 深度优先搜索

```js
class DFS {
  constructor(graph, s) {
    this.marked = new Array(graph.V).fill(false);
    this.count = 0;
    this.dfs(graph, s);
  }

  dfs(graph, v) {
    this.marked[v] = true;
    for (const w of graph.arj[v]) {
      if (!this.marked[v]) {
        this.dfs(graph, w);
      }
    }
  }
}
```

### 使用深度优先搜索寻找图中的路径

```js
/**
 * 使用深度优先搜索查找图中的路径
 */
class DFSPaths {
  constructor(graph, s) {
    this.marked = new Array(graph.V).fill(false);
    this.edgeTo = new Array(graph.V); // 从起点到一个顶点的已知路径上的最后一个顶点
    this.s = s; // 起点
    this.dfs(graph, s);
  }

  dfs(graph, v) {
    this.marked[v] = true;
    for (const w of graph.adj(v)) {
      if (!this.marked[w]) {
        this.edgeTo[w] = v;
        this.dfs(graph, w);
      }
    }
  }

  hasPathTo(v) {
    return this.marked[v];
  }

  pathTo(v) {
    if (!this.hasPathTo(v)) return null;
    const stack = [];
    for (let x = v; x !== this.s; x = this.edgeTo[x]) {
      stack.push(x);
    }
    stack.push(this.s);
    return stack;
  }
}
```

## 广度优先搜索

```js
/**
 * 使用广度优先搜索寻找图中的路径
 */
class BFSPaths {
  constructor(graph, s) {
    this.marked = new Array(graph.V).fill(false);
    this.edgeTo = [];
    this.s = s;
    this.bfs(graph, s);
  }

  bfs(graph, v) {
    const queue = [];
    this.marked[x] = true;
    queue.push(v);
    while (queue.length > 0) {
      const v = queue.shift();
      for (const w of graph.adj(v)) {
        if (!this.marked[w]) {
          queue.push(w);
          this.edgeTo[w] = v;
          this.marked[w] = true;
        }
      }
    }
  }

  hashPathTo(v) {
    return this.marked[v];
  }

  pathTo(v) {
    const stack = [];
    for (let x = v; x !== this.s; x = this.edgeTo[x]) {
      stack.push(x);
    }
    stack.push(this.s);
    return stack;
  }
}
```

> 将广度优先搜索中的队列换成栈就是非递归的深度优先搜索了（访问节点的顺序和基于递归的DFS有点差别）。BFS可以得到最优解，而DFS只能得到可行解。

## 连通分量

```js
/**
 * 深度优先搜索找出图中的所有连通分量
 */
class CC {
  constructor(graph) {
    this.marked = new Array(graph.V).fill(false);
    this._id = new Array(graph.V);
    this.count = 0;
    for (let s = 0; s < graph.V; s++) {
      if (!this.marked[s]) {
        this.dfs(graph, s);
        this.count++;
      }
    }
  }

  dfs(graph, v) {
    this.marked[v] = true;
    this._id[v] = this.count;
    for (const w of graph.adj(v)) {
      if (!this.marked) {
        this.dfs(graph, w);
      }
    }
  }

  connected(v, w) {
    return this._id[v] = this._id[w];
  }

  id(v) {
    return this._id[v];
  }
}
```

基于DFS的算法理论上比union-find算法更快，因为它能保证需要的时间是常数（V+E），但是在实际应用中union-find算法更快，因为它不需要完整构造并表示一幅图。更重要的是union-find算法是一种动态算法（我们在任何时间检查两个顶点是否连通，甚至是在添加一条边的时候），但是 **深度优先搜索必须对图进行预处理**。因此我们*在完成只需要判断连通性或是需要完成大量连通性查询和插入操作混合等类似任务的时候，更倾向于使用union-find算法，而深度优先搜索则更适合实现图的抽象数据类型，因为它能更有效地利用已有的数据结构*。

## 检测环路

https://www.jianshu.com/p/c95a7913e193

```js
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
```

## 判断是否为二分图

二分图指的是能够使用两种颜色将图的所有顶点着色，使得任意一条边的两个顶点的颜色都不相同。

```js
/**
 * 双色问题：使用DFS检测图是否为二分图
 */
class TwoColor {
  constructor(graph) {
    this.marked = new Array(graph.V()).fill(false);
    this.color = new Array(graph.V()).fill(false);
    this.isTwoColorable = true;
    for (let s = 0; s < graph.V(); s++) {
      if (!this.marked[s]) {
        this.dfs(graph, s);
      }
    }
  }

  dfs(graph, v) {
    this.marked[v] = true;
    for (const w of graph.adj(v)) {
      if (!this.marked[w]) {
        this.color[w] = !this.color[w];
        this.dfs(graph, w);
      } else if (this.color[w] === this.color[v]) {
        this.isTwoColorable = false;
        break;
      }
    }
  }
}
```

## 符号图

在典型的应用中图都是通过文件或者网页定义的，使用的是字符串而非整数来表示和指代顶点。为了适应这样的应用只需要建立字符串和整数的一一对应关系即可。SymbolGraph用到了以下的3种数据结构：

- 一个符号表st：键的类型为string（顶点名），值的类型为int（索引）
- 一个数组 keys[]:用做反向索引，保存每个顶点索引所对应的顶点名
- 一个Graph对象G。它使用索引来引用图中的顶点

![符号图中数据结构.png](http://git-hexo-blog.oss-cn-beijing.aliyuncs.com/%E7%AC%A6%E5%8F%B7%E5%9B%BE%E4%B8%AD%E6%95%B0%E6%8D%AE%E7%BB%93%E6%9E%84.png)

```js
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
```

![符号图示例.png](http://git-hexo-blog.oss-cn-beijing.aliyuncs.com/%E7%AC%A6%E5%8F%B7%E5%9B%BE%E7%A4%BA%E4%BE%8B.png)
