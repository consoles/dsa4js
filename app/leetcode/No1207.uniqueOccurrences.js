/**
 * @param {number[]} arr
 * @return {boolean}
 */
var uniqueOccurrences = function (arr) {
  // 计数
  const counter = new Map()
  for (const num of arr) {
    if (!counter.has(num)) {
      counter.set(num, 1)
    } else {
      counter.set(num, counter.get(num) + 1)
    }
  }
  // 判重
  const set = new Set()
  for (const count of counter.values()) {
    if (set.has(count)) {
      return false
    }
    set.add(count)
  }
  return true
};

arr = [1, 2, 2, 1, 1, 3]
console.log(uniqueOccurrences(arr))
arr = [1, 2]
console.log(uniqueOccurrences(arr))
arr = [-3, 0, 1, -3, 1, 1, 1, -3, 10, 0]
console.log(uniqueOccurrences(arr))
