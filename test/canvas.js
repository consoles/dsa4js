'use strict'

class Canvas {
  constructor(ctx) {
    this.ctx = ctx
    ctx.globalCompositeOperation = 'lighter'
  }
  drawPoint(x, y, radius, color) {
    var ctx = this.ctx
    ctx.fillStyle = color || '#00f'
    ctx.beginPath()
    ctx.arc(x, y, radius || 2, 0, 2 * Math.PI, true)
    ctx.closePath()
    ctx.fill()
  }
  drawRect(x, y, width, height, color) {
    var ctx = this.ctx
    ctx.fillStyle = color || '#0f0'
    ctx.fillRect(x, y, width, height)
  }
  drawLine(x1, y1, x2, y2, color, weight) {
    var ctx = this.ctx;
    ctx.strokeStyle = color || '#0ff'
    ctx.lineWidth = weight || 8
    ctx.beginPath()
    ctx.moveTo(x1, y1)
    ctx.lineTo(x2, y2)
    ctx.closePath()
    ctx.stroke()
  }
  fillText(text, x, y) {
    const ctx = this.ctx;
    ctx.fillStyle = '#f00';
    ctx.font = '20px Arial';
    ctx.fillText(text, x, y);
  }
  drawHorizontalMiddleLine(color, weight) {
    var ctx = this.ctx
    var canvas = ctx.canvas
    var x1 = 0
    var y1 = canvas.height / 2
    var x2 = x1 + canvas.width
    var y2 = y1
    // console.log(x1, y1, x2, y2)
    this.drawLine(x1, y1, x2, y2, color, weight)
  }
}