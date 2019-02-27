// 编写一个程序，计算输入文件中相等的整数对的数量。
// 如果你的第一个程序是平方级别的，
// 请继续思考并用 Array.sort() 给出一个线性对数级别的解答。

// 暴力搜索
const eqPairCount1 = nums => {
    let cnt = 0;
    for (let i = 0; i < nums.length; i++) {
        for (let j = i + 1; j < nums.length; j++) {
            if (nums[i] === nums[j]) {
                cnt++;
            }
        }
    }
    return cnt;
}

// 题目要求是线性对数级别，这个貌似是好于O(N^2),但是也不是线性对数级别
const eqPairCount2 = nums => {
    let cnt = 0;
    nums.sort((a, b) => a - b);
    for (let i = 0; i < nums.length; i++) {
        for (let j = i + 1; j < nums.length && nums[j] === nums[i]; j++) {
            cnt++;
        }
    }
    return cnt;
};

// 上面代码的进一步优化：https://github.com/ikesnowy/Algorithms-4th-Edition-in-Csharp/issues/437
// 我们只需要知道某个整数重复出现的次数，就能直接算出重复整数对的数量。
// 重复整数对 = 1 + 2 + 3 + ... + n-1 = n(n-1) / 2
// 因此我们只需要统计每个元素的出现次数，算出对应的重复整数对数量并相加。这个过程只需要遍历一遍数组，因为某个元素在内循环统计过之后就不需要再在外循环统计了。
// 内外循环可以共用一个临时变量。
const eqPairCount3 = nums => {
    let cnt = 0;
    let dup = 0; // 重复元素个数 - 1
    nums.sort((a, b) => a - b);
    for (let i = 0; i < nums.length - 1; i++) {
        while (nums[i + 1] === nums[i]) {
            dup++;
            i++;
        }
        cnt += dup * (dup + 1) / 2;
        dup = 0;
    }
    return cnt;
};

const nums = [1, 2, 3, 4, 1, 2, 1, 2, 1, 6];
let ret = eqPairCount1(nums);
ret = eqPairCount2(nums);
ret = eqPairCount3(nums);
debugger