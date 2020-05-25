// 圆圈中最后剩下的数字

// 0,1,,n-1这n个数字排成一个圆圈，从数字0开始，每次从这个圆圈里删除第m个数字。求出这个圆圈里剩下的最后一个数字。
//
// 例如，0、1、2、3、4这5个数字组成一个圆圈，从数字0开始每次删除第3个数字，则删除的前4个数字依次是2、0、4、1，因此最后剩下的数字是3。
//
// 来源：力扣（LeetCode）
// 链接：https://leetcode-cn.com/problems/yuan-quan-zhong-zui-hou-sheng-xia-de-shu-zi-lcof
//   著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。

// 约瑟夫环其实是有公式的

/**
 * @param {number} n
 * @param {number} m
 * @return {number}
 */
var lastRemaining = function (n, m) {

  // 初始化数组
  const arr = new Array(n);
  for (let i = 0; i < n; i++) {
    arr[i] = i;
  }

  let lastDel = null;
  let index = 0;
  while (arr.length) {
    for (let i = 0; i < m - 1; i++) {
      index = (index + 1) % arr.length;
    }
    lastDel = arr[index];
    // console.log(lastDel);
    arr.splice(index, 1);
  }

  return lastDel;
};

var lastRemaining2 = function (n, m) {

  // 初始化数组
  const arr = new Array(n);
  for (let i = 0; i < n; i++) {
    arr[i] = i;
  }

  let index = 0;
  while (n > 1) {
    index = (index + (m - 1)) % n;
    arr.splice(index, 1);
    n--;
  }

  return arr[0];
};

const ret = lastRemaining2(5, 3);
console.log(ret);
