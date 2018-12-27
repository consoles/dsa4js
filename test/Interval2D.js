'use strict'

/**
 * 2D间隔
 */
class Interval2D {

  constructor(interval1D_X, interval1D_Y) {
    this.interval1D_X = interval1D_X;
    this.interval1D_Y = interval1D_Y;
    this.width = interval1D_X.end - interval1D_X.start;
    this.height = interval1D_Y.end - interval1D_Y.start;
  }

  get offsetX() {
    return this.interval1D_X.start;
  }

  get offsetY() {
    return this.interval1D_Y.start;
  }

  area() {
    return this.width * this.height;
  }

  contains(point2D) {
    return this.interval1D_X.contains(point2D.x) && this.interval1D_Y.contains(point2D.y);
  }

  intersect(that) {
    return this.interval1D_X.intersection(that.interval1D_X) && this.interval1D_Y.intersection(that.interval1D_Y);
  }

  draw() {

  }
}