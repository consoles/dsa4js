/**
 * @param {number[]} nums
 * @param {number} target
 * @return {number[]}
 */
var searchRange = function (nums, target) {
    // 先用二分查找找出第一个元素的索引，然后用这个索引向左右两边进行扩散
    // let index = -1;
    // let l = 0, r = nums.length - 1;
    // while (l <= r) {
    //     const mid = l + parseInt((r - l) / 2);
    //     const num = nums[mid];
    //     if (num === target) {
    //         index = mid;
    //         break;
    //     }
    //     if (num < target) {
    //         l = mid + 1;
    //     } else {
    //         r = mid - 1;
    //     }
    // }
    // if (index === -1) {
    //     return [-1, -1];
    // }
    // let leftIndex = index;
    // let rightIndex = index;
    // while (leftIndex > 0 && nums[leftIndex - 1] === target) {
    //     leftIndex--;
    // }
    // while (rightIndex < nums.length - 1 && nums[rightIndex + 1] === target) {
    //     rightIndex++;
    // }
    // return [leftIndex, rightIndex];

    // 上面的算法在朴素的情况下复杂度确实是O(logN)的，但是对于数组中全是target的元素，复杂度退化为O(N)
    // 注意：下面的算法看起来像系统API，但是系统API可能出于通用性的考虑使用了线性查找
    function findFirstIndex(nums, target) {
        let l = 0, r = nums.length - 1;
        while (l <= r) {
            const mid = l + parseInt((r - l) / 2);
            const num = nums[mid];
            // 相等的时候并不跳出，而是继续向左边探测
            if (num === target) {
                r = mid - 1;
            } else if (num < target) {
                l = mid + 1;
            } else {
                r = mid - 1;
            }
        }
        if (l === nums.length || nums[l] !== target) {
            return -1;
        }
        return l;
    }
    function findLastIndex(nums, target) {
        let l = 0, r = nums.length - 1;
        while (l <= r) {
            const mid = l + parseInt((r - l) / 2);
            const num = nums[mid];
            if (num === target) {

            }
        }
    }
};

nums = [5, 7, 7, 8, 8, 10], target = 8
nums = [5, 7, 7, 8, 8, 10], target = 6
nums = [], target = 0
ret = searchRange(nums, target);
debugger