// 2.1.16
// 验证。
// 编写一个 check() 方法，调用 sort() 对任意数组排序。
// 如果排序成功而且数组中的所有对象均没有被修改则返回 true，否则返回 false。
// 不要假设 sort() 只能通过 exch() 来移动数据，可以信任并使用 Array.sort() 。

// 当交换数据的代价比克隆数据的代价大的时候，复制对象可能是更优的选择

const selectionSort = require('../../sort').selectionSort;

const check = arr => {
    const arr1 = arr.slice();
    const arr2 = arr.slice();

    selectionSort(arr1);
    arr2.sort((a, b) => a - b);

    for (let i = 0; i < arr1.length; i++) {
        if (arr1[i] !== arr2[i]) {
            return false;
        }
    }
    return true;
}

const arr = [1, 3, 2, 4, 1, 5, 6, 8, 7];
const flag = check(arr);
debugger