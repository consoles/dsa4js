/**
 * @param {number[]} bills
 * @return {boolean}
 */
var lemonadeChange = function (bills) {
    // 找零的情况分为3种：找零0，找零5，找零15(5 + 5 + 5或者10 + 5)
    // 当遇到找零15的时候我们总是贪心采用先找10再找5的方式
    // 10只能给20找零，但是5却能给10和20找零，5更万能，要留到最后用
    let remain5 = 0;
    let remain10 = 0;
    for (const bill of bills) {
        if (bill === 5) {
            // 无需找零
            remain5++;
        } else if (bill === 10) {
            // 找零5元
            if (remain5 <= 0) {
                return false;
            }
            remain10++;
            remain5--;
        } else if (bill === 20) {
            // 找零15元
            if (remain5 <= 0) {
                return false;
            }
            if (remain10 > 0) {
                // 找零 10 + 5
                remain10--;
                remain5--;
            } else {
                // 找零 5 * 3
                if (remain5 < 3) {
                    return false;
                }
                remain5 -= 3;
            }
        }
    }
    return true;
};

bills = [5, 5, 5, 10, 20];
// bills = [5, 5, 10];
// bills = [10, 10];
// bills = [5, 5, 10, 10, 20];
const res = lemonadeChange(bills)
debugger