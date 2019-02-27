// 编写一个程序，有序打印给定的两个有序数组（含有 N 个 int 值） 中的所有公共元素，
// 程序在最坏情况下所需的运行时间应该和 N 成正比。

const findSame1 = (arr1, arr2) => {
    for (const item1 of arr1) {
        for (const item2 of arr2) {
            if (item1 === item2) {
                console.log(item1, item2);
            }
        }
    }
}

const findSame2 = (arr1, arr2) => {
    let i = 0, j = 0;
    const len = arr1.length;
    while (i < len && j < len) {
        if (arr1[i] < arr2[j]) {
            i++;
        } else if (arr1[i] > arr2[j]) {
            j++;
        } else {
            console.log(i, j, arr1[i], arr2[j]);
            i++;
            j++;
        }
    }
};

const arr1 = [1, 1, 2, 3, 4];
const arr2 = [1, 1, 1, 2, 3];

const ret = findSame2(arr1, arr2);