// 三门问题（蒙题霍尔问题）

// 参赛者会看见三扇关闭的门，其中一扇的后面有一辆汽车，选中后面有车的那扇门就可以赢得该汽车，而另外两扇门后面则什么都没有。当参赛者选定一扇门，但开启的时候节目主持人会开启剩下两扇门的其中一扇，这扇门后面一定没有汽车。主持人问参赛者要不要换另一扇门。问题是：换另一扇门能否增加获奖概率？

const N = 1e7; // 模拟1000W次游戏

function play(changeDoor) {
  // 3扇门的编号分别是0，1，2
  const prizeDoor = Math.floor(Math.random() * 3);
  const playerChoice = Math.floor(Math.random() * 3);
  // 如果玩家选择的是有奖品的那扇门，一旦换了门就一定不会中奖
  if (playerChoice === prizeDoor) return !changeDoor;
  // 如果玩家的选择是没有奖品的那扇门，换门的话一定中奖（又排除了一扇没有奖品的门）
  return changeDoor;
}

function run(changeDoor) {
  let winCount = 0;
  for (let i = 0; i < N; i++) {
    if (play(changeDoor)) {
      winCount++;
    }
  }
  return winCount / N;
}

function threeGatesExperiment() {
  console.log('换门', run(true));
  console.log('不换门', run(false));
}

threeGatesExperiment();
