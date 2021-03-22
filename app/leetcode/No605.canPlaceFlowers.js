/**
 * @param {number[]} flowerbed
 * @param {number} n
 * @return {boolean}
 */
var canPlaceFlowers = function (flowerbed, n) {
    if (n === 0) {
        return true;
    }
    let count = 0;
    const len = flowerbed.length;
    for (let i = 0; i < len; i++) {
        const item = flowerbed[i];
        if (item === 0) {
            if (i === 0) {
                // 检查后面一个元素
                if (i + 1 < len && flowerbed[i + 1] === 0) {
                    count++;
                }
            } else if (i === len - 1) {
                // 检查前面一个元素
                if (i - 1 >= 0 && flowerbed[i - 1] === 0) {
                    count++;
                }
            } else if (flowerbed[i - 1] === 0 && flowerbed[i + 1] === 0) {
                // 检查前后两个元素
                count++;
            }
            if (count >= n) {
                return true;
            }
        }
    }
    return false;
};

flowerbed = [1, 0, 0, 0, 1], n = 1;
// flowerbed = [1, 0, 0, 0, 1], n = 2;
// flowerbed = [1, 0], n = 1;
let res = canPlaceFlowers(flowerbed, n);
debugger;
