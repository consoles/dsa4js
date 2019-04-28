// 2.1.14
// 出列排序。
// 说说你会如何将一副扑克牌排序，
// 限制条件是只能查看最上面的两张牌，交换最上面的两张牌，或是将最上面的一张牌放到这摞牌的最下面。

const sort = arr => {
    const cards = arr.slice();
    const ret = [];
    while (true) {
        // 经过n-1次操作后最小的牌放在了最前面，将此牌出列，不断找到最小的牌
        for (let i = 0; i < cards.length - 1; i++) {
            if (cards[0] < cards[1]) {
                [cards[0], cards[1]] = [cards[1], cards[0]];
            }
            cards.push(cards.shift());
            console.log(cards);
        }
        console.log('after change', cards);
        // 最大的牌被放在了最前面(出列)
        const min = cards.shift();
        ret.push(min);
        if (cards.length === 0) {
            break;
        }
    }
    return ret;
}

const arr = [4, 3, 1, 2, 5];
const ret = sort(arr);
console.log(ret);