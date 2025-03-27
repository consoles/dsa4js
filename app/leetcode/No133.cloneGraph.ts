/*Definition for _Node. */
class _Node {
    val: number
    neighbors: _Node[]

    constructor(val?: number, neighbors?: _Node[]) {
        this.val = (val === undefined ? 0 : val)
        this.neighbors = (neighbors === undefined ? [] : neighbors)
    }
}

function cloneGraph(node: _Node | null): _Node | null {
    const visited = new Map<number, _Node>()

    function dfs(node: _Node | null): _Node | null {
        if (node === null) {
            return null
        }
        const val = node.val
        if (visited.has(val)) {
            return visited.get(val)!
        }
        const newNode = new _Node(val)
        visited.set(val, newNode)
        for (const neighbor of node.neighbors) {
            newNode.neighbors.push(dfs(neighbor)!)
        }
        return newNode
    }
    return dfs(node)
};

function cloneGraph2(node: _Node | null): _Node | null {
    if (node === null) {
        return null
    }
    // 使用 BFS
    const visited = new Map<number, _Node>() // 源结点的值 -> 克隆结点
    const q = [node]
    const clone = new _Node(node.val)
    visited.set(node.val, clone)
    while (q.length) {
        const current = q.shift()
        const currentClone = visited.get(current.val)
        for (const neighbor of current.neighbors) {
            const val = neighbor.val
            // 如果没有访问过，则创建新节点,并加入到队列
            if (!visited.has(val)) {
                const neighborClone = new _Node(val)
                visited.set(val, neighborClone)
                q.push(neighbor)
            }
            currentClone.neighbors.push(visited.get(val)!)
        }
    }
    return clone
}

const node1 = new _Node(1)
const node2 = new _Node(2)
const node3 = new _Node(3)
const node4 = new _Node(4)
node1.neighbors = [node2, node4]
node2.neighbors = [node1, node3]
node3.neighbors = [node2, node4]
node4.neighbors = [node1, node3]

function printGraph(node: _Node | null) {
    const visited = new Map<number, boolean>()
    function dfs(node: _Node | null) {
        if (node === null) {
            console.log('null')
            return
        }
        const val = node.val
        if (visited.has(val)) {
            return // 已经访问过了
        }
        visited.set(val, true)
        console.log(node.val); // 打印当前节点
        // 打印所有邻居节点
        const neighborVals = node.neighbors.map(n => n.val)
        console.log('-->', neighborVals);
        for (const neighbor of node.neighbors) {
            dfs(neighbor)
        }
    }

    dfs(node)
}

console.log('原始图：')
printGraph(node1)
const res = cloneGraph2(node1)
console.log('克隆图：')
printGraph(res)
