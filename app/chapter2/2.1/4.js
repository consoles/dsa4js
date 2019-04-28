// 2.1.4
// 按照算法 2.2 所示轨迹的格式给出插入排序是如何将数组 E A S Y Q U E S T I O N 排序的。

// 插入排序的
//    E A S Y Q U E S T I O N
// 1  A E
// 2  A E S    
// 3  A E S Y
// 4  A E Q S Y
// 5  A E Q S U Y
// 6  A E E Q S U Y
// 7  A E E Q S S U Y
// 8  A E E Q S S T U Y
// 9  A E E I Q S S T U Y
// 10 A E E I O Q S S T U Y
// 11 A E E I N O Q S S T U Y

const insertSort = arr => {
    for (let i = 1; i < arr.length; i++) {
        for (let j = i; j > 0 && arr[j] < arr[j - 1]; j--) {
            [arr[j], arr[j - 1]] = [arr[j - 1], arr[j]];
        }
        console.log(arr);
    }
    console.log(arr);
};

const arr = 'E A S Y Q U E S T I O N'.split('').filter(x => x.trim().length > 0);
insertSort(arr);

// ['A', 'E', 'S', 'Y', 'Q', 'U', 'E', 'S', 'T', 'I', 'O', 'N']
// ['A', 'E', 'S', 'Y', 'Q', 'U', 'E', 'S', 'T', 'I', 'O', 'N']
// ['A', 'E', 'S', 'Y', 'Q', 'U', 'E', 'S', 'T', 'I', 'O', 'N']
// ['A', 'E', 'Q', 'S', 'Y', 'U', 'E', 'S', 'T', 'I', 'O', 'N']
// ['A', 'E', 'Q', 'S', 'U', 'Y', 'E', 'S', 'T', 'I', 'O', 'N']
// ['A', 'E', 'E', 'Q', 'S', 'U', 'Y', 'S', 'T', 'I', 'O', 'N']
// ['A', 'E', 'E', 'Q', 'S', 'S', 'U', 'Y', 'T', 'I', 'O', 'N']
// ['A', 'E', 'E', 'Q', 'S', 'S', 'T', 'U', 'Y', 'I', 'O', 'N']
// ['A', 'E', 'E', 'I', 'Q', 'S', 'S', 'T', 'U', 'Y', 'O', 'N']
// ['A', 'E', 'E', 'I', 'O', 'Q', 'S', 'S', 'T', 'U', 'Y', 'N']
// ['A', 'E', 'E', 'I', 'N', 'O', 'Q', 'S', 'S', 'T', 'U', 'Y']
// ['A', 'E', 'E', 'I', 'N', 'O', 'Q', 'S', 'S', 'T', 'U', 'Y']