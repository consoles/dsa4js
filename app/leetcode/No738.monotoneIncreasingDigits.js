/**
 * @param {number} N
 * @return {number}
 */
var monotoneIncreasingDigits = function (N) {
    // 最简单的思路，从当前数开始每次减1，查看这个数是不是单调递增
    // function isIncrease(n) {
    //     // 每次取最低位，应该是递减序列
    //     let prev = Number.MAX_SAFE_INTEGER;
    //     while (n > 0) {
    //         const num = n % 10;
    //         if (num > prev) {
    //             return false;
    //         }
    //         n = parseInt(n / 10);
    //         prev = num;
    //     }
    //     return true;
    // }

    // while (N > 0) {
    //     if (isIncrease(N)) return N;
    //     N--;
    // }
    // return N;

    // 上面的不断尝试的思路会超时，如果能直接构造出这样的一个数那应该是最快的
    // 从后向前遍历，如果当前的值大于后面的值就把当前的值减去1，然后把后面的数变成9
    if (N < 10) return N;
    const arr = [];
    while (N) {
        arr.unshift(N % 10);
        N = parseInt(N / 10);
    }
    for (let i = arr.length - 2; i > -1; i--) {
        if (arr[i] > arr[i + 1]) {
            arr[i]--;
            for (let j = i + 1; j < arr.length; j++) {
                arr[j] = 9;
            }
        }
    }
    let sum = 0;
    for (const num of arr) {
        sum = sum * 10 + num;
    }
    return sum;
};

N = 10;
N = 332;
// N = 1234;
// N = 882930776;
const res = monotoneIncreasingDigits(N);
debugger;