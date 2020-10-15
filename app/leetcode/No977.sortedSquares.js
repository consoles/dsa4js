/**
 * @param {number[]} A
 * @return {number[]}
 */
var sortedSquares = function(A) {
    // 最朴素的写法，计算出每个数组的平方，然后再排序
    // const squares = A.map(x => x**2).sort((a,b) => a-b)
    // return squares

    // 方法2：由于数组升序（并且包含负数，最大的平方数一定位于数组的左右两端），使用双指针每次放入最大的即可
    let i = 0,j = A.length - 1
    const res = new Array(A.length)
    let pos = j;
    while(i <= j) {
        const startS = A[i] ** 2
        const endS = A[j] ** 2
        if (startS > endS) {
            i++
            res[pos] = startS
        } else {
            j--
            res[pos] = endS
        }
        pos--
    }
    return res
};

console.log(sortedSquares([-4,-1,0,3,10]));
console.log(sortedSquares([-7,-3,2,3,11]));
