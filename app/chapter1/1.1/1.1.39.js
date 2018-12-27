/*
* 1.1.39
* 
* 随机匹配。
* 编写一个使用 BinarySearch 的程序，
* 它从命令行接受一个整型参数 T，
* 并会分别针对 N = 10^3、10^4、10^5 和 10^6 将以下实验运行 T 遍：
* 生成两个大小为 N 的随机 6 位正整数数组并找出同时存在于两个数组中的整数的数量。
* 打印一个表格，对于每个 N，给出 T 次实验中该数量的平均值。* 
*/

const _ = require('lodash');

const { binarySearch } = require('../../binarySearch');

const test = t => {

    const times = [1e3, 1e4, 1e5, 1e6];
    for (let time of times) {
        let count = 0;
        for (let i = 0; i < t; i++) {
            // init array
            let arr1 = new Array(time);
            let arr2 = new Array(time);
            for (let i = 0; i < time; i++) {
                arr1[i] = _.random(1e5 + 1, 1e6 - 1);
                arr2[i] = _.random(1e5 + 1, 1e6 - 1);
            }
            // 注意：务必排序
            arr1.sort((a, b) => a - b);
            // find common count
            for (let i = 0; i < time; i++) {
                if (binarySearch(arr1, arr2[i]) !== -1) {
                    count++;
                }
            }
        }
        console.log(t, time, count / t);
    }
}

test(10);
test(20);
test(30);
test(40);
test(50);

// 10 1000 0.7
// 10 10000 108.4
// 10 100000 10553.2
// 10 1000000 671027.7
// 20 1000 1.1
// 20 10000 111.25
// 20 100000 10502.55
// 20 1000000 670890
// 30 1000 0.9333333333333333
// 30 10000 108.2
// 30 100000 10523.5
// 30 1000000 670752.6333333333
// 40 1000 1.1
// 40 10000 111.225
// 40 100000 10526.025
// 40 1000000 670736.075
// 50 1000 1.06
// 50 10000 108.84
// 50 100000 10527
// 50 1000000 670793.78