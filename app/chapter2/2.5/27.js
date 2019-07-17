function indirectInsertionSort(arr) {
  const indexes = [];
  for (let i = 0; i < arr.length; i++) {
    indexes.push(i);
  }
  for (let i = 1; i < arr.length; i++) {
    for (let j = i; j > 0 && arr[indexes[j]] < arr[indexes[j - 1]]; j--) {
      [indexes[j], indexes[j - 1]] = [indexes[j - 1], indexes[j]];
    }
  }
  return indexes;
}

const arr = [2, 1, 4, 3, 5, 7, 6];
const ret = indirectInsertionSort(arr);
debugger;
