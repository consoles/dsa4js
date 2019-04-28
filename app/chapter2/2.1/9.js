// 2.1.9
// 按照算法 2.3 所示轨迹的格式给出希尔排序是如何将数组
// E A S Y S H E L L S O R T Q U E S T I O N

// n = 21,n / 3 = 7
// h = 13

// E A E S S H E L L S O R T Q U S Y T I O N

// h = 4

// E A E L 
// L H E O 
// N Q I R
// S S O S 
// T T U S 
// Y

// E A E L L H E O N Q I R S S O S T T U S Y

// h = 1
// A E E E H I L L N O O Q R S S S S T T U Y 

const swap = require('../../swap');

let shellSort = arr => {
    const n = arr.length;
    let h = 1;
    while (h < n / 3) {
        h = 3 * h + 1; // 1,4,13,40...
    }
    while (h >= 1) {
        for (let i = h; i < n; i++) {
            for (let j = i; j >= h && arr[j] < arr[j - h]; j -= h) {
                swap(arr, j, j - h);
            }
        }
        console.log('h = ', h, 'arr = ', JSON.stringify(arr));
        h = parseInt(h / 3);
    }
}

const arr = 'E A S Y S H E L L S O R T Q U E S T I O N'.split(/\s+/);

shellSort(arr);

// h =  13 arr =  ["E","A","E","S","S","H","E","L","L","S","O","R","T","Q","U","S","Y","T","I","O","N"]
// h =  4 arr =  ["E","A","E","L","L","H","E","O","N","Q","I","R","S","S","O","S","T","T","U","S","Y"]
// h =  1 arr =  ["A","E","E","E","H","I","L","L","N","O","O","Q","R","S","S","S","S","T","T","U","Y"]