/**
 * @param {number} N
 * @param {number[][]} edges
 * @return {number[]}
 */
var sumOfDistancesInTree = function (N, edges) {
    // 根据边数组构造邻接表，然后按照图进行bfs
    // const graph = new Array(N)
    // for (let i = 0; i < N; i++) {
    //     graph[i] = []
    // }
    // for (const [start, end] of edges) {
    //     // 注意无向图邻接表是双向的
    //     graph[start].push(end)
    //     graph[end].push(start)
    // }
    // const res = []
    // for (let i = 0; i < N; i++) {
    //     // 以i为起点，进行BFS，计算步数的和
    //     const visited = new Array(N).fill(false)
    //     let depth = 0
    //     let sum = 0
    //     const q = [i]
    //     visited[i] = true
    //     while (q.length) {
    //         let size = q.length
    //         depth++
    //         while (size--) {
    //             const index = q.shift()
    //             for (const v of graph[index]) {
    //                 if (!visited[v]) {
    //                     sum += depth
    //                     q.push(v)
    //                     visited[v] = true
    //                 }
    //             }
    //         }
    //     }
    //     res.push(sum)
    // }
    // return res

    // 上面算法的正确性应该没问题，64 / 69 个通过测试用例,最后几个用例超时了
    const graph = new Array(N)
    for (let i = 0; i < N; i++) {
        graph[i] = []
    }
    for (const [start, end] of edges) {
        graph[start].push(end)
        graph[end].push(start)
    }

    const distSum = new Array(N).fill(0)
    const nodeNum = new Array(N).fill(1)

    // 求root到子树所有节点的距离和
    function postOrder(root, parent) {
        for (const neighbor of graph[root]) {
            if (neighbor === parent) continue
            postOrder(neighbor, root)
            nodeNum[root] += nodeNum[neighbor]
            distSum[root] += nodeNum[neighbor] + distSum[neighbor]
        }
    }

    // 根据root计算其邻居到所在子树之外的节点的距离和（包括root节点）
    function preOrder(root, parent) {
        for (const neighbor of graph[root]) {
            if (neighbor === parent) continue
            distSum[neighbor] = distSum[root] - nodeNum[neighbor] + (N - nodeNum[neighbor])
            preOrder(neighbor, root)
        }
    }

    postOrder(0, -1)
    preOrder(0, -1)
    return distSum
};

N = 6, edges = [[0, 1], [0, 2], [2, 3], [2, 4], [2, 5]]
console.log(sumOfDistancesInTree(N, edges));