/**
 * @param {number} num
 * @return {string}
 */
var convertToBase7 = function (num) {
    let res = '';
    const radix = 7;
    let isNagative = false;
    if (num < 0) {
        isNagative = true;
        num = -num;
    }
    while (num >= radix) {
        res = num % radix + '' + res;
        num = parseInt(num / radix);
    }
    res = num + '' + res;
    if (isNagative) {
        res = '-' + res;
    }
    return res;
};

let res = convertToBase7(100);
res = convertToBase7(-7);
debugger