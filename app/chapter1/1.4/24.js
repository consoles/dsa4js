// 扔鸡蛋。假设你面前有一栋 N 层的大楼和许多鸡蛋，假设将鸡蛋从 F 层或者更高的地方扔下鸡蛋才会摔碎，否则则不会。首先，设计一种策略来确定 F 的值，其中扔 ~lgN 次鸡蛋后摔碎的鸡蛋数量为 ~lgN。然后想办法将成本降低到~2lgF

const F = 5; // 假设从5楼往下扔是会碎的临界值

// N >= 5
const throwEggs = N => {
    let lo = 1;
    let hi = N;

    let count = 0;
    let brokenCount = 0;

    while (lo <= hi) {
        const mid = lo + Math.floor((hi - lo) / 2);
        count++;
        if (mid > F) {
            // 鸡蛋可以碎
            hi = mid - 1;
            brokenCount++;
        } else if (mid === F) {
            brokenCount++;
            hi = mid;
            break;
        } else {
            lo = mid + 1;
        }
    }
    console.log('F = ', hi, '一共扔了', count, '次鸡蛋,碎了', brokenCount, '个');
    return hi;
}

const throwEggs2 = N => {
    let lo = 1;
    let hi = 1;
    let count = 0;
    while (hi < F) {
        lo = hi;
        hi *= 2;
        count++;
    }
    if (hi === F) return;

    let brokenCount = 0;

    while (lo <= hi) {
        const mid = lo + Math.floor((hi - lo) / 2);
        count++;
        if (mid > F) {
            // 鸡蛋可以碎
            hi = mid - 1;
            brokenCount++;
        } else if (mid === F) {
            brokenCount++;
            hi = mid;
            break;
        } else {
            lo = mid + 1;
        }
    }
    console.log('F = ', hi, '一共扔了', count, '次鸡蛋,碎了', brokenCount, '个');
    return hi;
}

throwEggs(1000);
throwEggs2(1000);