// 按照算法 2.1 所示轨迹的格式给出选择排序是如何将数组 E A S Y Q U E S T I O N 排序的。

// 选择排序的
//    E A S Y Q U E S T I O N
// 1  A       E S Y Q U E S T I O N
// 2  A E     S Y Q U E S T I O N
// 3  A E E   S Y Q U S T I O N
// 4  A E E I  S Y Q U S T O N
// 5  A E E I N - S Y Q U S T O
// 6  A E E I N O - S Y Q U S T
// 7  A E E I N O Q - S Y U S T
// 8  A E E I N O Q S - Y U S T
// 9  A E E I N O Q S S - Y U T
// 10 A E E I N O Q S S T - Y U
// 11 A E E I N O Q S S T U - Y 

let selectionSort = arr => {
    for (let i = 0; i < arr.length; i++) {
        let minIndex = i
        for (let j = i + 1; j < arr.length; j++) {
            if (arr[j] < arr[minIndex]) {
                minIndex = j
            }
        }
        [arr[i], arr[minIndex]] = [arr[minIndex], arr[i]];
        console.log(arr);
    }
    console.log(arr);
    return arr
}

const arr = 'E A S Y Q U E S T I O N'.split('').filter(x => x.trim().length > 0);
selectionSort(arr);

// ['A', 'E', 'S', 'Y', 'Q', 'U', 'E', 'S', 'T', 'I', 'O', 'N']
// ['A', 'E', 'S', 'Y', 'Q', 'U', 'E', 'S', 'T', 'I', 'O', 'N']
// ['A', 'E', 'E', 'Y', 'Q', 'U', 'S', 'S', 'T', 'I', 'O', 'N']
// ['A', 'E', 'E', 'I', 'Q', 'U', 'S', 'S', 'T', 'Y', 'O', 'N']
// ['A', 'E', 'E', 'I', 'N', 'U', 'S', 'S', 'T', 'Y', 'O', 'Q']
// ['A', 'E', 'E', 'I', 'N', 'O', 'S', 'S', 'T', 'Y', 'U', 'Q']
// ['A', 'E', 'E', 'I', 'N', 'O', 'Q', 'S', 'T', 'Y', 'U', 'S']
// ['A', 'E', 'E', 'I', 'N', 'O', 'Q', 'S', 'T', 'Y', 'U', 'S']
// ['A', 'E', 'E', 'I', 'N', 'O', 'Q', 'S', 'S', 'Y', 'U', 'T']
// ['A', 'E', 'E', 'I', 'N', 'O', 'Q', 'S', 'S', 'T', 'U', 'Y']
// ['A', 'E', 'E', 'I', 'N', 'O', 'Q', 'S', 'S', 'T', 'U', 'Y']
// ['A', 'E', 'E', 'I', 'N', 'O', 'Q', 'S', 'S', 'T', 'U', 'Y']
// ['A', 'E', 'E', 'I', 'N', 'O', 'Q', 'S', 'S', 'T', 'U', 'Y']