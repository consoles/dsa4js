// 一式三份

// 给定3个列表，每个列表中包含N个名字，使用线性对数级别的算法来判定3份列表中是否包含公共的名字，如果有，返回第一个被找到的这种名字

function findCommon1(list1, list2, list3) {
  for (const item1 of list1) {
    for (const item2 of list2) {
      for (const item3 of list3) {
        if (item1 === item2 && item1 === item3) {
          return item1;
        }
      }
    }
  }
  return null;
}

const {mergeSort} = require('../../sort');
const {binarySearch} = require('../../binarySearch');

// 归并排序 + 二分查找
function findCommon2(list1, list2, list3) {
  // 使用归并排序排列3个列表
  list1 = mergeSort(list1);
  list2 = mergeSort(list2);
  list3 = mergeSort(list3);

  // 扫描list1，并使用二分查找在list2和list3中寻找是否有相同元素
  for (const item of list1) {
    if (binarySearch(list2, item) !== -1 && binarySearch(list3, item) !== -1) {
      return item;
    }
  }
  return null;
}

const list1 = [1, 5, 3, 2, 4];
const list2 = [1, 7, 2, 8, 5];
const list3 = [4, 5, 7, 1, 8];

const ret = findCommon2(list1, list2, list3);
debugger;
