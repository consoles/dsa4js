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

console.log(checkIsValidSeq([3, 2, 1, 5, 4]))
console.log(checkIsValidSeq([3, 1, 2, 5, 4]))