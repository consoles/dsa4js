const robot = require('robotjs');
// Speed up the mouse.
robot.setMouseDelay(2);

// const screenSize = robot.getScreenSize();

function sleep(ms) {
  return new Promise((resolve => setTimeout(resolve, ms)));
}

(async () => {

  const mouse = robot.getMousePos();
  console.log('鼠标当前位置', mouse);

  // 激活任务栏游戏窗口
  console.log('激活游戏');
  const gameIconX = 222;
  const gameIconY = 878;
  robot.moveMouse(gameIconX, gameIconY);
  robot.mouseClick()

  let c = 0;
  const count = 4;
  const gameCenterMapX = 509;
  const gameCenterMapY = 307;
  // 捡一圈东西
  while (true) {
    console.log('回到游戏起始坐标', `(${gameCenterMapX},${gameCenterMapY})`);
    robot.moveMouse(gameCenterMapX, gameCenterMapY); // 游戏中的位置
    for (const dir of ['w', 'a', 's', 'd']) {
      for (let i = 0; i < count; i++) {
        robot.keyTap(dir);
        await sleep(Math.floor(20 + Math.random() * 1000))
        robot.keyTap('z')
      }
      await sleep(Math.floor(20 + Math.random() * 1000))
      robot.keyTap('2')
    }
    console.log('运行次数', c++)
    await sleep(2000)
  }
})();
