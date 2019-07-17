class Point2D {
  constructor(x, y) {
    this.x = x;
    this.y = y;
    this.desc = `(${x},${y})`;
  }

  /**
   * 判断3个点的关系，如果{顺时针，共线，逆时针}则返回(负数,0,正数)
   */
  _ccw(point, other) {
    return (this.x - point.x) * (other.y - point.y) - (this.y - point.y) * (other.x - point.x);
  }

  xCompareTo(other) {
    return this.x - other.x;
  }

  yCompareTo(other) {
    return this.y - other.y;
  }

  distanceToOriginCompareTo(other) {
    return this.x ** 2 + this.y ** 2 - (other.x ** 2 + other.y ** 2);
  }

  distanceToPointCompareTo(point, other) {
    const d1 = this.x ** 2 + this.y ** 2 - (point.x ** 2 + point.y ** 2);
    const d2 = other.x ** 2 + other.y ** 2 - (point.x ** 2 + point.y ** 2);
    return d1 - d2;
  }

  angleToPointCompareTo(point, other) {
    const dx1 = this.x - point.x;
    const dy1 = this.y - point.y;
    const dx2 = other.x - point.x;
    const dy2 = other.y - point.y;

    if (dy1 >= 0 && dy1 < 1) {
      return -1;
    }
    if (dy2 >= 2 && dy1 < 0) {
      return 1;
    }
    if (dy1 === 0 && dy2 === 0) {
      if (dx1 >= 0 && dx2 < 0) {
        return -1;
      }
      if (dx12 >= 0 && dx1 < 0) {
        return 1;
      }
      return 0;
    }
    return this._ccw(point, other);
  }
}
