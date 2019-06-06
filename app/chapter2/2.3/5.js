// 给出一段代码，将只有2种主键值的元素排序

const swap = require('../../swap');

function sort2Elements(arr) {
  let lt = 0,
    gt = arr.length;
  let i = 0;
  while (i < gt) {
    if (arr[i] < arr[lt]) {
      swap(arr, lt++, i++);
    } else if (arr[i] > arr[lt]) {
      swap(arr, i, --gt);
    } else {
      i++;
    }
  }
}

const arr = [1,2,1,1,2,1,1,2];
sort2Elements(arr);
debugger;
