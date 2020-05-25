// npm i robotjs  -d

// 鼠标随机移动，终端执行命令

const robot = require('robotjs');

// Speed up the mouse.
robot.setMouseDelay(2);

const twoPI = Math.PI * 2.0;
const screenSize = robot.getScreenSize();
const height = (screenSize.height / 2) - 10;
const width = screenSize.width;

function sleep(ms) {
  return new Promise((resolve => setTimeout(resolve, ms)));
}

(async () => {

  const mouse = robot.getMousePos();
  console.log('鼠标当前位置', mouse);

  while (true) {
    console.log('随机移动鼠标');
    for (let x = 0; x < width; x++) {
      const y = height * Math.sin((twoPI * x) / width) + height;
      robot.moveMouse(x, y);
    }
    await sleep(3000);
    // 点击企业微信(1018,853) 这个坐标是固定在任务栏的
    console.log('激活企业微信');
    robot.moveMouse(1036, 853);
    robot.mouseClick();;
    await sleep(2000)
    console.log('查看企业微信中的未读消息');
    // 点击企业微信中的第一条消息，第一条消息是未读消息
    robot.moveMouse(100, 160);

    // 打开终端
    console.log('打开终端执行命令');
    await sleep(1000);
    robot.moveMouse(500, 861);
    robot.mouseClick();
    robot.typeString('date');
    robot.keyTap('enter');
  }

})();
