// 假设某个用例程序会进行一系列入栈和出栈操作。入栈操作会将整数0到9按顺序压入栈；出栈操作会打印返回值,求所有可能出现的序列
const getAllSeq = nums => {

    const len = nums.length;
    const res = [];

    const dfs = (index, part, stack) => {
        if (index === len && stack.length === 0) {
            res.push(part.slice());
            return;
        }
        // 入栈
        if (index < len) {
            stack.push(nums[index]);
            dfs(index + 1, part, stack);
            stack.pop();
        }
        // 出栈
        if (stack.length > 0) {
            const top = stack.pop();
            part.push(top);
            dfs(index, part, stack);
            stack.push(top);
            part.pop();
        }
    };

    dfs(0, [], []);

    return res;
};

// const ret = getAllSeq([1, 2, 3]).map(arr => arr.join(''));
// console.log(ret);

const swap = require('../../swap');

// 全排列
// 123 => 123,132,231,213,312,321 一共6个
const permutation = nums => {

    const res = [];
    const len = nums.length;

    // 1 + {2,3}的全排列 2 + {1,3}的全排列 3 + {1,3}的全排列
    const dfs = (index, part) => {
        if (index === len) {
            res.push(part.slice());
            return;
        }
        for (let i = index; i < len; i++) {
            // 将当前索引和index交换
            swap(part, i, index);
            dfs(index + 1, part);
            // 再交换回来
            swap(part, index, i);
        }
    };

    dfs(0, nums);

    return res;
};

const checkIsValidSeq = seq => {
    // 合法序列：出栈序列中的每一个数字，比它小的数字，一定是按照递减顺序排列的
    const check = index => {
        const cur = seq[index];
        const afterLessThan = seq.slice(index).filter(x => x < cur);
        for (let i = 0; i < afterLessThan.length - 1; i++) {
            let c = afterLessThan[i];
            let n = afterLessThan[i + 1];
            if (c < n) {
                return false;
            }
        }
        return true;
    }
    for (let i = 0; i < seq.length - 1; i++) {
        if (!check(i)) {
            return false;
        }
    }
    return true;
};

const getAllSeq2 = nums => permutation(nums).filter(seq => checkIsValidSeq(seq));

const ret = getAllSeq2([1, 2, 3]);
debugger