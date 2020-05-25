// 22. 括号生成

// 数字 n 代表生成括号的对数，请你设计一个函数，用于能够生成所有可能的并且 有效的 括号组合。
//
//  
//
// 示例：
//
// 输入：n = 3
// 输出：[
//   "((()))",
//   "(()())",
//   "(())()",
//   "()(())",
//   "()()()"
// ]
//
// 来源：力扣（LeetCode）
// 链接：https://leetcode-cn.com/problems/generate-parentheses
//   著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。

/**
 * @param {number} n
 * @return {string[]}
 */
var generateParenthesis = function (n) {
  // 方案1：获取数组全排列，判断全排列中合法的括号
  // const arr = [];
  // while (n--) {
  //   arr.push('(');
  //   arr.push(')');
  // }
  //
  // function isValid(arr) {
  //   let balance = 0;
  //   for (const c of arr) {
  //     if (c === '(') {
  //       balance++;
  //     } else if (c === ')') {
  //       balance--;
  //       if (balance < 0) return false;
  //     }
  //   }
  //   return balance === 0;
  // }
  //
  // const set = new Set();
  //
  // function dfs(index) {
  //   if (index === arr.length) {
  //     set.add(arr.join(''));
  //     return;
  //   }
  //   for (let i = index; i < arr.length; i++) {
  //     [arr[index], arr[i]] = [arr[i], arr[index]];
  //     dfs(index + 1);
  //     [arr[index], arr[i]] = [arr[i], arr[index]];
  //   }
  // }
  //
  // dfs(0);
  //
  // const ret = [];
  //
  // for (const item of set) {
  //   if (isValid(item.split(''))) {
  //     ret.push(item);
  //   }
  // }
  //
  // return ret;

  // 方案2：DFS
  // const res = [];

  // function dfs2(curStr, leftUsed, rightUsed) {
  //   if (leftUsed === n && rightUsed === n) {
  //     res.push(curStr);
  //     return;
  //   }
  //   if (leftUsed < rightUsed) {
  //     return;
  //   }
  //   if (leftUsed < n) {
  //     dfs2(curStr + '(', leftUsed + 1, rightUsed);
  //   }
  //   if (rightUsed < n) {
  //     dfs2(curStr + ')', leftUsed, rightUsed + 1);
  //   }
  // }

  // /**
  //  * @param curStr 从根节点到当前节点的路径字符串
  //  * @param leftRemain 左括号还可以使用的个数
  //  * @param rightRemain 右括号还可以使用的个数
  //  */

  // function dfs(curStr, leftRemain, rightRemain) {
  //   if (leftRemain === 0 && rightRemain === 0) {
  //     res.push(curStr);
  //     return;
  //   }
  //   // 剪枝（如图，左括号可以使用的个数严格大于右括号可以使用的个数，才剪枝，注意这个细节）
  //   if (leftRemain > rightRemain) {
  //     return;
  //   }
  //   if (leftRemain > 0) {
  //     dfs(curStr + '(', leftRemain - 1, rightRemain);
  //   }
  //   if (rightRemain > 0) {
  //     dfs(curStr + ')', leftRemain, rightRemain - 1);
  //   }
  // }
  //
  // dfs('', n, n);

  // 方案三：BFS
  // class Node {
  //   constructor(str, leftRemain, rightRemain) {
  //     this.str = str;
  //     this.leftRemain = leftRemain;
  //     this.rightRemain = rightRemain;
  //   }
  // }
  //
  // const res = [];
  //
  // const q = [new Node('', n, n)];
  // while (q.length) {
  //   const {str, leftRemain, rightRemain} = q.shift();
  //   if (leftRemain === 0 && rightRemain === 0) {
  //     res.push(str);
  //   }
  //   if (leftRemain > rightRemain) {
  //     continue;
  //   }
  //   if (leftRemain > 0) {
  //     q.push(new Node(str + '(', leftRemain - 1, rightRemain));
  //   }
  //   if (rightRemain > 0) {
  //     q.push(new Node(str + ')', leftRemain, rightRemain - 1));
  //   }
  // }
  // return res;
};

let res = generateParenthesis(10);
console.log(res);
