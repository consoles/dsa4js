/**
 * @param {number[]} nums
 * @return {boolean}
 */
var containsDuplicate = function (nums) {
    const seen = new Set();
    for (const num of nums) {
        if (seen.has(num)) {
            return true;
        }
        seen.add(num);
    }
    return false;
};

// const flag = containsDuplicate([1, 2, 3, 1])
// const flag = containsDuplicate([1, 2, 3, 4])
const flag = containsDuplicate([1, 1, 1, 3, 3, 4, 3, 2, 4, 2])
debugger