/**
 * @param {number[]} nums
 * @return {number[][]}
 */
var subsets = function(nums) {
    const paths = []
    function dfs(start,path){
        paths.push(path.slice())
        for(let i = start;i < nums.length;i++) {
            path.push(nums[i])
            dfs(i + 1,path)
            path.pop()
        }
    }
    dfs(0,[])
    return paths
};

// 迭代法实现子集枚举
var subsets = function(nums) {
    const ans = [];
    const n = nums.length;
    for (let mask = 0; mask < (1 << n); ++mask) {
        const t = [];
        for (let i = 0; i < n; ++i) {
            if (mask & (1 << i)) {
                t.push(nums[i]);
            }
        }
        ans.push(t);
    }
    return ans;
};

nums = [1,2,3]
console.log(subsets(nums))