/**
 * 最大水容器
 *
 * @param {number[]} height
 * @return {number}
 */
var maxArea = function (height) {

  // 暴力法
  let area = 0;
  // for (let i = 0; i < height.length - 1; i++) {
  //   for (let j = i + 1; j < height.length; j++) {
  //     area = Math.max(area, (j - i) * Math.min(height[i], height[j]));
  //   }
  // }

  // 一开始两个指针一个指向开头一个指向结尾，此时容器的底是最大的，接下来随着指针向内移动，会造成容器的底变小，在这种情况下想要让容器盛水变多，就只有在容器的高上下功夫。 那我们该如何决策哪个指针移动呢？我们能够发现不管是左指针向右移动一位，还是右指针向左移动一位，容器的底都是一样的，都比原来减少了 1。这种情况下我们想要让指针移动后的容器面积增大，就要使移动后的容器的高尽量大，所以我们选择指针所指的高较小的那个指针进行移动，这样我们就保留了容器较高的那条边，放弃了较小的那条边，以获得有更高的边的机会。

  // 双指针（每次移动高度较小的指针），因为如果移动高度较大的指针，则矩形的宽减小，高度不变导致面积变小，移动高度较小的指针则可能产生更大的面积
  // 双指针法其实可以看做是暴力穷举的剪枝，可以看成每一根柱子和其他所有柱子都进行了匹配，但是忽略了很多明显不可能结果的情况
  let l = 0, r = height.length - 1;
  while (l < r) {
    area = Math.max(area, (r - l) * Math.min(height[l], height[r]));
    if (height[l] < height[r]) {
      l++;
    } else {
      r--;
    }
  }

  return area;
};

maxArea([1, 8, 6, 2, 5, 4, 8, 3, 7]);
