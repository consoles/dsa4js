// 你一定会中奖么？
// 打开一个宝箱获得传奇武器的概率是20%，那么打开5个这样的宝箱就一定能获得传奇武器么

const chance = 0.2;
const playTime = 5;
const n = 1e6;

function play() {
  for (let i = 0; i < playTime; i++) {
    if (Math.random() < chance) {
      return true;
    }
  }
  return false;
}

function run() {
  let winCount = 0;
  for (let i = 0; i < n; i++) {
    if (play()) {
      winCount++;
    }
  }
  return winCount / n;
}

function winning_prize() {
  // 1 - 0.8^5 = 0.672
  console.log(`开${playTime}个宝箱中奖概率`, run());
  // 1 - 0.8^x > 0.95通过概率反解开多少次宝箱比较大概率能获得传奇武器，需要承担一定的风险
  // 1 / 0.2 = 5其实是数学期望，平均来看，打开5个宝箱能获得传奇武器
}

winning_prize();
