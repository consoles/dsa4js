// 优惠券收集问题。用和上一题相同的方式生成随机整数。通过实验验证生成所有可能的整数值所需生成的随机数总量为 N*调和级数。

/**
 * 调和级数求和
 * 1 + 1/2 + 1/3 + ... + 1/n
 */
const harmonicSum = n => {
    let sum = 0;
    for (let i = 1; i <= n; i++) {
        sum += 1 / i;
    }
    return sum;
};

const _ = require('lodash');

const N = 1e4;
const arr = new Array(N);
let sum = 0;
const times = 20;
for (let time = 0; time < times; time++) {
    arr.fill(false);
    for (let i = 0; true; i++) {
        const index = _.random(0, N - 1);
        arr[index] = true;
        if (arr.every(x => x)) {
            sum += i;
            console.log(`生成${i}次后所有可能都出现过了`);
            break;
        }
    }
}
console.log(`平均生成${sum / times}个数字后出现所有可能，N*Hn = ${N * harmonicSum(N)}`);

// 生成99626次后所有可能都出现过了
// 生成98942次后所有可能都出现过了
// 生成109831次后所有可能都出现过了
// 生成104684次后所有可能都出现过了
// 生成89015次后所有可能都出现过了
// 生成102728次后所有可能都出现过了
// 生成100808次后所有可能都出现过了
// 生成95179次后所有可能都出现过了
// 生成106713次后所有可能都出现过了
// 生成91963次后所有可能都出现过了
// 生成89826次后所有可能都出现过了
// 生成79206次后所有可能都出现过了
// 生成116631次后所有可能都出现过了
// 生成86503次后所有可能都出现过了
// 生成119622次后所有可能都出现过了
// 生成110192次后所有可能都出现过了
// 生成95149次后所有可能都出现过了
// 生成99456次后所有可能都出现过了
// 生成88877次后所有可能都出现过了
// 生成91964次后所有可能都出现过了
// 平均生成98845.75个数字后出现所有可能，N * Hn = 97876.06036044349