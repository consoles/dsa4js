/**
 * @param {number[]} nums
 * @return {number}
 */
var majorityElement = function(nums) {
    const halfLen = nums.length / 2;
    const counter = new Map();
    for(const num of nums) {
        let count = 0
        if (!counter.has(num)) {
            count = 1;
        } else {
            count = counter.get(num) + 1
        }
        counter.set(num,count)
        if (count > halfLen) {
            return num
        }
    }
};

const nums = [1, 2, 3, 2, 2, 2, 5, 4, 2]
const res = majorityElement(nums)
console.log(res);