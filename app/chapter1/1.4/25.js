// 但现在假设你只有两个鸡蛋，而你的成本模型则是扔鸡蛋的次数。设计一种策略，最多扔 2√(N) 次鸡蛋即可判断出 F 的值， 然后想办法把这个成本降低到 ~c√(F) 次。 这和查找命中（鸡蛋完好无损）比未命中（鸡蛋被摔碎）的成本小得多的情形类似。

const F = 5;

const throwEggs3 = N => {

    let lo = 1;
    let hi = 1;
    let count = 0;
    let brokenCount = 0;

    while (hi < F) {
        lo = hi;
        count++;
        hi += Math.sqrt(N);
    }
    brokenCount++;
    hi = Math.min(hi, N);
    if (hi === F) return F;

    // 不能用二分查找，找到一个比较小的值，然后向右推进
    while (lo <= hi) {
        count++;
        if (lo >= F) {
            brokenCount++;
            break;
        }
        lo++;
    }
    console.log('一共扔了', count, '次，碎了', brokenCount, '个蛋，找到F= ', F);
};

const throwEggs4 = N => {
    let lo = 1;
    let hi = 1;
    let count = 0;
    let brokenCount = 0;

    for (let i = 0; hi < F; i++) {
        count++;
        lo = hi;
        hi += i;
    }
    brokenCount++;
    hi = Math.min(hi, N);
    while (lo <= hi) {
        count++;
        if (lo >= F) {
            brokenCount++;
            break;
        }
        lo++;
    }
    console.log('一共扔了', count, '次，碎了', brokenCount, '个蛋，找到F= ', F);
}

throwEggs3(1000);
throwEggs4(1000);
