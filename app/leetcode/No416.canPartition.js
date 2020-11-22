/**
 * @param {number[]} nums 非空正整数集合
 * @return {boolean}
 */
var canPartition = function(nums) {
    let sum = 0
    for(const num of nums){
        sum += num
    }
    // 如果和不是偶数，则一定不能分割成两个子集的和
    if (sum % 2 !== 0) return false

    const targetSum = sum / 2
    const n = nums.length

    const memo = new Map()

    function _canPartition(curSum,index) {
        if (curSum === targetSum) return true
        if (curSum > targetSum || index >= n) {
            return false
        }
        const key = index +':' + curSum
        if (memo.has(key)) {
            return memo.get(key)
        }
        // 选nums[index]，则当前和为curSum + nums[index];不选curSum，则当前和还是curSum
        // 两种情况下都需要考虑下一个元素
        const flag = _canPartition(curSum + nums[index],index + 1) || _canPartition(curSum,index+1)
        memo.set(key,flag)
        return flag
    }

    return _canPartition(0,0)
};

let flag = canPartition([1, 5, 11, 5])
flag = canPartition([1, 2, 3, 5])
flag = canPartition([1, 2, 5])
debugger