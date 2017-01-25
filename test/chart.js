'use strict'

class Chart{
  constructor(canvas,xScale,yScale){
    this.canvas = canvas
    this.xScale = xScale
    this.yScale = yScale

    this.bounds = {
      left:0,
      top:0,
      right:600,
      bottom:600
    }

    // 精度1/10000
    this.accuracy = {
      x:1e-4,
      y:1e-4
    }

    var pixPerPartX = (this.bounds.right - this.bounds.left) * this.accuracy.x
    var pixPerPartY = (this.bounds.bottom - this.bounds.top) * this.accuracy.y

    this.pixPerPart = {
      x:pixPerPartX,
      y:pixPerPartY
    }
  }

  getRandomColor(){
    return "#"+("00000"+((Math.random()*16777215+0.5)>>0).toString(16)).slice(-6); 
  }

  draw(point) {
    var x = point.x,
    y = point.y,
    r = point.r,
    color = point.color

    // 判断点落在第几个区间
    var countX = x / this.accuracy.x
    var countY = y / this.accuracy.y

    var posX = this.bounds.left + this.pixPerPart.x * countX
    var posY = this.bounds.bottom - this.pixPerPart.y * countY

    this.canvas.drawPoint(posX,posY,r,color)
    this.canvas.drawLine(this.bounds.left,this.bounds.bottom,posX,posY,'#f00',1)
  }

  /**
   * 画单位矩形的子集
   */
  drawRect(x,y){
    // 判断点落在第几个区间
    var countX = x / this.accuracy.x
    var countY = y / this.accuracy.y

    var w = this.pixPerPart.x * countX
    var h = this.pixPerPart.y * countY
    var posX = this.bounds.left
    var posY = this.bounds.bottom - h 

    // 确定矩形的左上角坐标和宽高即可
    this.canvas.drawRect(posX,posY,w,h,this.getRandomColor())
  }

}