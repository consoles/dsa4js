/**
 * @param {number[][]} edges
 * @return {number[]}
 */
var findRedundantConnection = function (edges) {
    // 在一棵树中，边的数量比节点的数量少1.如果一棵树有N个节点，那么这棵树有N-1条边。
    // 这道题中的图在树的基础上多了一条附加的边，因此边的数量也是N
    // 可以通过并查集查找附加的边。初始的时候每个节点属于不同的联通分量。遍历每一条边，判断这条边的两个连接是否是同一个联通分量

    // quick union
    function union(p, q) {
        const pRoot = find(p);
        const qRoot = find(q);
        parent[pRoot] = qRoot;
    }

    function find(p) {
        // if (parent[p] !== p) {
        //     parent[p] = find(parent[p]);
        // }
        // return parent[p];
        while (p !== parent[p]) {
            p = parent[p];
        }
        return p;
    }

    const nodesCount = edges.length;
    const parent = new Array(nodesCount + 1);
    for (let i = 1; i <= nodesCount; i++) {
        parent[i] = i;
    }
    for (const edge of edges) {
        const [node1, node2] = edge;
        if (find(node1) === find(node2)) {
            return edge;
        }
        union(node1, node2);
    }
    return edges[0];
};

const edges = [[1, 2], [2, 3], [3, 4], [1, 4], [1, 5]];
const res = findRedundantConnection(edges);
debugger;
