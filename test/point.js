class Point2D {
  constructor(x, y, r, color) {
    this.x = x
    this.y = y
    this.r = r || 5
    this.color = color || "#" + ("00000" + ((Math.random() * 16777215 + 0.5) >> 0).toString(16)).slice(-6)
  }

  // 欧氏距离
  distance(point) {
    const offsetX = this.x - point.x;
    const offsetY = this.y - point.y;
    return Math.sqrt(offsetX ** 2 + offsetY ** 2);
  }
}