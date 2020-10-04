/**
 * @param {number} n
 * @param {number[]} left
 * @param {number[]} right
 * @return {number}
 */
var getLastMoment = function (n, left, right) {
    // 由于改变移动方向不花费额外时间，而且改变移动方向后的移动速度不变，因此，两只相遇的蚂蚁同时改变移动方向之后的情形等价于两只蚂蚁都不改变移动方向，继续按照原来的方向和速度移动，这样问题就简化成根据每只蚂蚁的初始位置和移动方向得到最后一只蚂蚁到达木板边界的时刻。
    // 碰撞掉头等于穿透，可以理解为穿透爬行，题目就变成了求单只最晚落地的蚂蚁，与碰撞无关
    // 这一题如果去分析每只蚂蚁，情况将会变得非常复杂
    let t = 0
    for (const ant of left) {
        t = Math.max(ant, t)
    }
    for (const ant of right) {
        t = Math.max(n - ant, t)
    }
    return t
};

n = 4, left = [4, 3], right = [0, 1]
n = 7, left = [], right = [0, 1, 2, 3, 4, 5, 6, 7]
n = 7, left = [0, 1, 2, 3, 4, 5, 6, 7], right = []
n = 9, left = [5], right = [4]
n = 6, left = [6], right = [0]
console.log(getLastMoment(n, left, right));