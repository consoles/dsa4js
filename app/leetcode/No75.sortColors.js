/**
 * @param {number[]} nums
 * @return {void} Do not return anything, modify nums in-place instead.
 */
var sortColors = function (nums) {
    // 思路1，计数排序
    // const counter = new Array(3).fill(0)
    // for (const num of nums) {
    //     counter[num]++
    // }
    // let j = 0
    // for (let i = 0; i < counter.length; i++) {
    //     let count = counter[i]
    //     while (count--) {
    //         nums[j++] = i
    //     }
    // }

    function swap(i, j) {
        [nums[i], nums[j]] = [nums[j], nums[i]]
    }

    // 荷兰国旗问题
    // 思路2：两次遍历
    // const n = nums.length
    // let ptr = 0
    // // 将0放到正确的位置
    // for (let i = 0; i < n; i++) {
    //     if (nums[i] === 0) {
    //         swap(i, ptr++)
    //     }
    // }
    // // 将1放到正确的位置，剩下的2自然而然位于正确的位置
    // for (let i = ptr; i < n; i++) {
    //     if (nums[i] === 1) {
    //         swap(i, ptr++)
    //     }
    // }

    // 思路3：双指针
    let p0 = 0, p2 = nums.length - 1, cur = 0
    while (cur <= p2) {
        const num = nums[cur]
        if (num === 0) {
            swap(cur, p0)
            cur++
            p0++
        } else if (num === 2) {
            swap(cur, p2)
            // 注意：！！！
            // cur++
            p2--
        } else {
            cur++
        }
    }
};

const nums = [1, 2, 0]
sortColors(nums)
console.log(nums);