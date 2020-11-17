/**
 * @param {number} R
 * @param {number} C
 * @param {number} r0
 * @param {number} c0
 * @return {number[][]}
 */
var allCellsDistOrder = function (R, C, r0, c0) {
    // 方法1：构建邻接表，然后使用广度优先搜索
    // const q = [[r0, c0]]
    // const result = [];
    // const visited = new Array(R)
    // for (let i = 0; i < visited.length; i++) {
    //     visited[i] = new Array(C).fill(false);
    // }
    // const dirs = [[0, 1], [1, 0], [0, -1], [-1, 0]];
    // while (q.length) {
    //     let size = q.length;
    //     while (size--) {
    //         const item = q.shift();
    //         if (visited[item[0]][item[1]]) continue;
    //         result.push(item);
    //         visited[item[0]][item[1]] = true;
    //         const [x, y] = item;
    //         for (const [xOffset, yOffset] of dirs) {
    //             const newX = x + xOffset;
    //             const newY = y + yOffset;
    //             if (newX >= 0 && newX < R && newY >= 0 && newY < C) {
    //                 if (!visited[newX][newY]) {
    //                     q.push([newX, newY]);
    //                 }
    //             }
    //         }
    //     }
    // }
    // return result;

    // 方法2：直接排序
    // const items = [];
    // for (let x = 0; x < R; x++) {
    //     for (let y = 0; y < C; y++) {
    //         // 计算曼哈顿距离
    //         const dis = Math.abs(x - r0) + Math.abs(y - c0);
    //         items.push({ dis, points: [x, y] })
    //     }
    // }
    // items.sort((a, b) => a.dis - b.dis);
    // return items.map(x => x.points);

    // 方法3：桶排序
    // 1. 遍历所有坐标，按照距离的大小进行分组，每一组的距离相等（放入到相同的桶中）
    // 2. 按照距离从小到大的原则，遍历所有的桶，并输出结果
    // 桶的个数为距离的最大值：行，列距离的最大值,时间复杂度为O(R*C)，理论最快
    const bucketCount = Math.max(r0, R - 1 - r0) + Math.max(c0, C - 1 - c0) + 1;
    const buckets = [];
    for (let i = 0; i < bucketCount; i++) {
        buckets[i] = [];
    }
    for (let x = 0; x < R; x++) {
        for (let y = 0; y < C; y++) {
            const dis = Math.abs(x - r0) + Math.abs(y - c0);
            buckets[dis].push([x, y]);
        }
    }
    const result = [];
    for (const arr of buckets) {
        for (const item of arr) {
            result.push(item);
        }
    }
    return result;
};

R = 1, C = 2, r0 = 0, c0 = 0
R = 2, C = 2, r0 = 0, c0 = 1
R = 2, C = 3, r0 = 1, c0 = 2
let res = allCellsDistOrder(R, C, r0, c0)
debugger