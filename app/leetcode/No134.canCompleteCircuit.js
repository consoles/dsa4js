/**
 * @param {number[]} gas
 * @param {number[]} cost
 * @return {number}
 */
var canCompleteCircuit = function (gas, cost) {
    const len = gas.length;
    // 方法1，暴力探测
    // function detect(startIndex) {
    //     let remainGas = 0;
    //     let i = startIndex;
    //     while (true) {
    //         remainGas += gas[i]; // 加油
    //         // 开向下一个加油站
    //         remainGas -= cost[i];
    //         if (remainGas < 0) {
    //             return false;
    //         }
    //         i = (i + 1) % len;
    //         if (i === startIndex) {
    //             return true;
    //         }
    //     }
    // }
    // for (let i = 0; i < len; i++) {
    //     const flag = detect(i);
    //     if (flag) {
    //         return i;
    //     }
    // }
    // return -1;

    // 方法2：使用折线图的思想巧解
    // https://leetcode-cn.com/problems/gas-station/solution/shi-yong-tu-de-si-xiang-fen-xi-gai-wen-ti-by-cyayc/
    // let spare = 0; // 当前剩余油量
    // let minSpare = Number.MAX_SAFE_INTEGER; // 剩余油量最小值
    // let minIndex = 0; // 剩余油量最小值索引
    // for (let i = 0; i < len; i++) {
    //     spare += (gas[i] - cost[i]);
    //     if (spare < minSpare) {
    //         minSpare = spare;
    //         minIndex = i;
    //     }
    // }
    // // 跑完一圈后，若剩余油量为负数，意味着一圈的总消耗大于总补给，不可能跑完一圈
    // //  再来说下为什么代码里面，返回的是 indexMin+1 ，因为这里的索引值与图解里并不是一样的，图解是为了更好地去讲解思路，而代码中的索引是为了代码服务的，两个索引值并不对应。你可以发现，代码中，每次更新i的时候，剩余油量已经更新到下一个点了。例如i=0时，循环中的剩余油量 gasSurplus += gas[0] - cost[0]实际上是到达第二个点的剩余油量。这导致了最小值与索引值不匹配（差一位），所以输出时输出的是indexMin+1。 至于%len(gas)是为了处理超出索引的情况，其实就是落在最后一点时， indexMin+1取第一个点。
    // return spare < 0 ? -1 : (minIndex + 1) % len;

    // 另一个清奇的思路：如果总的加油量大于总的耗油量，则问题一定有解
    // 方法3：
    // https://leetcode-cn.com/problems/gas-station/solution/shou-hua-tu-jie-liang-ge-guan-jian-jie-lun-de-jian/
    let left = 0, start = 0, totalGas = 0, totalCost = 0;
    for (let i = 0; i < gas.length; i++) {
        totalGas += gas[i];
        totalCost += cost[i];
        left += gas[i] - cost[i];
        if (left < 0) {
            start = i + 1;
            left = 0;
        }
    }
    if (totalGas < totalCost) {
        return -1;
    }
    return start;
};

gas = [1, 2, 3, 4, 5]
cost = [3, 4, 5, 1, 2]

// gas = [2, 3, 4]
// cost = [3, 4, 3]
// let res = canCompleteCircuit(gas, cost);
debugger;