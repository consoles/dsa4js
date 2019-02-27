// 1.4.22 仅用加减实现的二分查找（Mihai Patrascu）。编写一个程序，给定一个含有 N 个不同 int 值的按照升序排列的数组，判断它是否含有给定的整数。只能使用加法和减法以及常数的额外内存空间。程序的运行时间在最坏情况下应该和 logN 成正比。提示：用斐波那契数代替2的幂（二分法）进行查找。用两个变量保存Fk和Fk - 1并在[i, i + Fk]之间查找。在每一步中，使用减法计算Fk - 2，检查i + Fk - 2处的元素，并根据结果将搜索范围变为[i, i + Fk - 2]或是[i + Fk - 2, i + Fk - 2 + Fk - 1]。

/**
 * 创建最大值刚好>=待查找数组长度的裴波纳契数组  
 */
const makeFibArray = arr => {
    const len = arr.length;
    let first = 1;
    let second = 1;
    const a = [first, second];
    while (true) {
        const current = first + second;
        a.push(current);
        if (current >= len) {
            break;
        }
        first = second;
        second = current;
    }
    return a;
}

const fibSearch = (nums, num) => {
    const fibArr = makeFibArray(nums); // 斐波那契数组
    const filledLength = fibArr[fibArr.length - 1]; // 填充数组长度
    // 构建填充数组：填充数组长度为大于等于待查找数组长度向上取整的斐波那契数
    // 前一部分为待查找数组，后部分用原数组的最后一个元素填充
    const filledArray = new Array(filledLength);
    for (let i = 0; i < nums.length; i++) {
        filledArray[i] = nums[i];
    }
    const last = nums[nums.length - 1];
    for (let i = nums.length; i < filledLength; i++) {
        filledArray[i] = last;
    }

    let lo = 0;
    let hi = arr.length - 1;
    let k = fibArr.length - 1; // 用来控制子数组的左右边界
    while (lo <= hi) {
        const mid = lo + fibArr[k - 1] - 1;
        if (num < filledArray[mid]) {
            hi = mid - 1;
            k = k - 1;
        } else if (num > filledArray[mid]) {
            lo = mid + 1;
            k = k - 2;
        } else {
            return mid > hi ? hi : mid;
        }
    }
    return -1;
}

const arr = [1, 2, 3, 4, 5, 6, 7, 8, 9];
const index = fibSearch(arr, 8);
debugger