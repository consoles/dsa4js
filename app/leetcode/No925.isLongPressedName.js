/**
 * @param {string} name
 * @param {string} typed
 * @return {boolean}
 */
var isLongPressedName = function (name, typed) {
  // 双指针
  // if (typed.length < name.length) return false
  // let i = 0 // 指向name
  // let j = 0 // 指向typed
  // while (j < typed.length) {
  //   if (typed[j] !== name[i]) {
  //     if (j > 0 && typed[j] === typed[j - 1]) {
  //       j++
  //       continue
  //     }
  //     return false
  //   }
  //   i++
  //   j++
  // }
  // // 第一个字符串是否遍历到末尾
  // return i === name.length

  let i = 0, j = 0
  while (j < typed.length) {
    if (i < name.length && name[i] === typed[j]) {
      i++
      j++
    } else if (j > 0 && typed[j] === typed[j - 1]) {
      j++
    } else {
      return false
    }
  }
  return i === name.length
};

let res = isLongPressedName("alex", "aaleex")
res = isLongPressedName("saeed", 'ssaaedd')
res = isLongPressedName("leelee", 'lleeelee')
res = isLongPressedName("laiden", "laiden")
res = isLongPressedName('pyplrz', 'ppyypllr')

debugger
