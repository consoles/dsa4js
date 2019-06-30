// 使用蒙特卡洛算法求pi

const N = 1e8;
const squareSide = 800;

// 圆的外接矩形的面积 squareSide ^ 2
// 圆的面积 pi * (squareSide / 2) ^ 2

// 圆的面积 / 矩形面积 = pi / 4
// 圆内的点数 / 一共的点数 = pi / 4

function experiment() {
  const xC = squareSide / 2;
  const yC = squareSide / 2;
  const r = squareSide / 2;
  let count = 0;
  for (let i = 0; i < N; i++) {
    const x = Math.random() * squareSide;
    const y = Math.random() * squareSide;
    // 点在圆内？
    if ((x - xC) ** 2 + (y - yC) ** 2 <= r ** 2) {
      count++;
    }
  }
  return count / N * 4;
}

const pi = experiment();
console.log(pi);
