'use strict'

// canvas坐标系左上角(0,0)这个修改为笛卡尔坐标系，左下角是(0,0)
class Chart {
  constructor(canvas, xScale = 1, yScale = 1) {
    this.canvas = canvas;
    this.xScale = xScale;
    this.yScale = yScale;

    this.bounds = {
      left: 0,
      top: 0,
      right: 600,
      bottom: 600
    };

    const pixPerPartX = (this.bounds.right - this.bounds.left) / this.xScale;
    const pixPerPartY = (this.bounds.bottom - this.bounds.top) / this.yScale;

    this.pixPerPart = {
      x: pixPerPartX,
      y: pixPerPartY
    }
  }

  getRandomColor() {
    return "#" + ("00000" + ((Math.random() * 16777215 + 0.5) >> 0).toString(16)).slice(-6);
  }

  draw(point) {
    const x = point.x,
      y = point.y,
      r = point.r,
      color = point.color;

    const posX = this.bounds.left + this.pixPerPart.x * x;
    const posY = this.bounds.bottom - this.pixPerPart.y * y;

    this.canvas.drawPoint(posX, posY, r, color);
    this.canvas.fillText(`${point.text}`, posX, posY);
    // this.canvas.drawLine(this.bounds.left, this.bounds.bottom, posX, posY, '#f00', 1)
  }

  drawLine(pointA, pointB, lineWidth = 1) {
    const posAX = this.bounds.left + this.pixPerPart.x * pointA.x;
    const posAY = this.bounds.bottom - this.pixPerPart.y * pointA.y;
    const posBX = this.bounds.left + this.pixPerPart.x * pointB.x;
    const posBY = this.bounds.bottom - this.pixPerPart.y * pointB.y;
    this.canvas.drawLine(posAX, posAY, posBX, posBY, '#000', lineWidth);
  }

  /**
   * 画单位矩形的子集
   */
  drawRect(width, height, left = 0, bottom = 0) {
    const w = this.pixPerPart.x * width;
    const h = this.pixPerPart.y * height;

    const posX = this.bounds.left + this.pixPerPart.x * left;
    const posY = this.bounds.bottom - h - this.pixPerPart.y * bottom;

    // 确定矩形的左上角坐标和宽高即可
    this.canvas.drawRect(posX, posY, w, h, this.getRandomColor());
    this.canvas.fillText(`${w} * ${h}`, posX, posY);
  }

}
