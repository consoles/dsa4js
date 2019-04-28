// 寻找数组中的逆序对
const reversePair = arr => {
    const n = arr.length;
    const pairs = [];
    for (let i = 0; i < n; i++) {
        for (let j = i + 1; j < n; j++) {
            if (arr[i] > arr[j]) {
                pairs.push([arr[i], arr[j]]);
            }
        }
    }
    return pairs;
};

const arr = 'EXAMPLE'.split('');
const ret = reversePair(arr);
debugger