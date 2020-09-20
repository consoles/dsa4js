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

nums = [1,2,3]
console.log(subsets(nums))