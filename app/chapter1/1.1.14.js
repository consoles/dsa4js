// 编写一个静态方法lg(),接收参数M，返回不大于log2M的最大整数，不能使用math库

const lg = num => {
    let ret = 0;
    while (num > 1) {
        num = parseInt(num / 2);
        ret++;
    }
    return ret;
}

const ret = lg(15);
console.log(ret);